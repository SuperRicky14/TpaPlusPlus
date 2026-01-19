package net.superricky.tpaplusplus.io

import java.util.UUID

data class PlayerData(var tPToggle: Boolean = false) {
    private val blockedPlayers: MutableSet<UUID> = hashSetOf()

    fun addBlockedPlayer(player: UUID) {
        blockedPlayers.add(player)
    }

    fun removeBlockedPlayer(player: UUID) {
        blockedPlayers.remove(player)
    }

    fun hasBlockedPlayer(player: UUID): Boolean {
        return blockedPlayers.contains(player)
    }
}
