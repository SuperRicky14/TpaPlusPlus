package net.superricky.tpaplusplus.database

import net.superricky.tpaplusplus.GlobalConst
import org.jetbrains.exposed.dao.IntEntity
import org.jetbrains.exposed.dao.IntEntityClass
import org.jetbrains.exposed.dao.id.EntityID
import org.jetbrains.exposed.dao.id.IntIdTable

@Suppress("StringLiteralDuplication")
object Tables {
    object Players : IntIdTable("${GlobalConst.MOD_ID.lowercase()}_players") {
        val playerId = uuid("player_id").uniqueIndex("player_id")
        val playerName = char("player_name", DatabaseConst.MAX_PLAYER_NAME_LENGTH)
        val blockAll = bool("block_all").default(false)
    }

    class Player(id: EntityID<Int>) : IntEntity(id) {
        var playerId by Players.playerId
        var playerName by Players.playerName
        var blockAll by Players.blockAll

        companion object : IntEntityClass<Player>(Players)
    }

    object BlockedPlayers : IntIdTable("${GlobalConst.MOD_ID.lowercase()}_blocked_players") {
        val playerId = reference("player_id", Players.playerId)
        val playerName = reference("player_name", Players.playerName)
        val blockedPlayerId = reference("blocked_player_id", Players.playerId)
        val blockedPlayerName = reference("blocked_player_name", Players.playerName)

        init {
            index("uuid_index_blocked_players", false, playerId, blockedPlayerId)
        }
    }

    class BlockedPlayer(id: EntityID<Int>) : IntEntity(id) {
        var playerId by Player referencedOn BlockedPlayers.playerId
        var playerName by Player referencedOn BlockedPlayers.playerName
        var blockedPlayerId by Player referencedOn BlockedPlayers.blockedPlayerId
        var blockedPlayerName by Player referencedOn BlockedPlayers.blockedPlayerName

        companion object : IntEntityClass<BlockedPlayer>(BlockedPlayers)
    }

    object LastDeaths : IntIdTable("${GlobalConst.MOD_ID.lowercase()}_last_deaths") {
        val playerId = reference("player_id", Players.playerId)
        val deathWorld = char("death_world", DatabaseConst.MAX_WORLD_LENGTH)
        val x = double("x")
        val y = double("y")
        val z = double("z")
        val backed = bool("backed").default(false)

        init {
            index("uuid_index_last_deaths", false, playerId)
        }
    }

    class LastDeath(id: EntityID<Int>) : IntEntity(id) {
        var playerId by Player referencedOn LastDeaths.playerId
        var deathWorld by LastDeaths.deathWorld
        var x by LastDeaths.x
        var y by LastDeaths.y
        var z by LastDeaths.z
        var backed by LastDeaths.backed

        companion object : IntEntityClass<LastDeath>(LastDeaths)
    }
}
