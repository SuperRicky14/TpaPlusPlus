@file:UseSerializers(UUIDSerializer::class)
package net.superricky.tpaplusplus.io

import kotlinx.serialization.Serializable
import kotlinx.serialization.UseSerializers
import java.util.UUID

@Serializable
data class PlayerData(val tpToggle: Boolean = DEFAULT_TP_TOGGLE_STATE, val blockedPlayers: Set<UUID> = setOf()) {
    fun hasBlockedPlayer(player: UUID): Boolean {
        return blockedPlayers.contains(player)
    }

}
