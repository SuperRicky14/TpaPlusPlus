package net.superricky.tpaplusplus.database

import kotlinx.coroutines.delay
import net.superricky.tpaplusplus.GlobalConst.logger
import net.superricky.tpaplusplus.TpaPlusPlus
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.database.DatabaseConst.MAX_QUERY_RETRIES
import net.superricky.tpaplusplus.database.DatabaseConst.MAX_RETRY_DELAY
import net.superricky.tpaplusplus.database.DatabaseConst.MIN_RETRY_DELAY
import org.jetbrains.exposed.sql.*
import org.jetbrains.exposed.sql.statements.StatementContext
import org.jetbrains.exposed.sql.statements.expandArgs
import org.jetbrains.exposed.sql.transactions.experimental.newSuspendedTransaction
import org.jetbrains.exposed.sql.transactions.transaction
import org.sqlite.SQLiteConfig
import org.sqlite.SQLiteDataSource
import java.util.*
import javax.sql.DataSource
import kotlin.io.path.pathString

object DatabaseManager {
    private lateinit var database: Database

    private val sqlLogger = object : SqlLogger {
        override fun log(context: StatementContext, transaction: Transaction) {
            logger.info("SQL: ${context.expandArgs(transaction)}")
        }
    }
    val databaseType: String
        get() = database.dialect.name

    private val cache = DatabaseCacheService

    fun setup() {
        database = Database.connect(getDefaultDatasource())
    }

    private fun getDefaultDatasource(): DataSource {
        val dbFilepath = Config.getDatabasePath().resolve("tpaplusplus.sqlite").pathString
        return SQLiteDataSource(
            SQLiteConfig().apply {
                setJournalMode(SQLiteConfig.JournalMode.WAL)
            }
        ).apply {
            url = "jdbc:sqlite:$dbFilepath"
        }
    }

    fun ensureTables() = transaction {
        addLogger(sqlLogger)
        SchemaUtils.createMissingTablesAndColumns(
            Tables.Players,
            Tables.BlockedPlayers,
            withLogs = true
        )
        logger.info("Tables created")
    }

    private suspend fun <T : Any?> execute(body: suspend Transaction.() -> T): T {
        while (TpaPlusPlus.server.overworld?.savingDisabled != false) {
            delay(timeMillis = 1000)
        }

        return newSuspendedTransaction(db = database) {
            maxAttempts = MAX_QUERY_RETRIES
            minRetryDelay = MIN_RETRY_DELAY
            maxRetryDelay = MAX_RETRY_DELAY

            addLogger(sqlLogger)
            body(this)
        }
    }

    suspend fun setupCache() {
        execute {
            Tables.Player.all().forEach {
                cache.playerKeys.put(it.playerId, it.id.value)
            }
        }
    }

    suspend fun insertPlayer(uuid: UUID, name: String) =
        execute {
            val player = Tables.Player.find { Tables.Players.playerId eq uuid }.firstOrNull()
            if (player == null) {
                Tables.Player.new {
                    this.playerId = uuid
                    this.playerName = name
                }
            }
        }

    suspend fun playerSwitchBlock(uuid: UUID) =
        execute {
            val player = Tables.Player.find { Tables.Players.playerId eq uuid }.firstOrNull()
            if (player == null) {
                throw NullPointerException("Player $uuid not found")
            }
            val blocked = player.blockAll
            player.blockAll = !blocked
            return@execute !blocked
        }

    suspend fun playerSwitchBlock(uuid: UUID, blocked: Boolean) =
        execute {
            val player = Tables.Player.find { Tables.Players.playerId eq uuid }.firstOrNull()
            player?.let {
                if (player.blockAll == blocked) return@let
                player.blockAll = blocked
            }
        }

    suspend fun insertBlockedPlayer(senderUUID: UUID, targetUUID: UUID) =
        execute {
            val sender = Tables.Player.find { Tables.Players.playerId eq senderUUID }.firstOrNull()
            val target = Tables.Player.find { Tables.Players.playerId eq targetUUID }.firstOrNull()
            if (sender == null || target == null) {
                return@execute false
            }
            Tables.BlockedPlayer.new {
                playerId = sender
                playerName = sender
                blockedPlayerId = target
                blockedPlayerName = target
            }
            return@execute true
        }

    suspend fun deleteBlockedPlayer(senderUUID: UUID, targetUUID: UUID) =
        execute {
            val blocked = Tables.BlockedPlayer.find {
                Tables.BlockedPlayers.playerId eq senderUUID and (Tables.BlockedPlayers.blockedPlayerId eq targetUUID)
            }
                .firstOrNull()
            if (blocked == null) {
                return@execute false
            }
            blocked.delete()
            return@execute true
        }
}
