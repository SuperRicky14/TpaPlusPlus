@file:UseSerializers(UUIDSerializer::class)
package net.superricky.tpaplusplus.io

import kotlinx.serialization.Serializable
import kotlinx.serialization.UseSerializers
import java.util.UUID

@Serializable
data class PlayerData(val tpToggle: Boolean = DEFAULT_TP_TOGGLE_STATE, internal val blockedPlayers: MutableSet<UUID> = hashSetOf()) {
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
