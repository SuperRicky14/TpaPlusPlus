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
}
