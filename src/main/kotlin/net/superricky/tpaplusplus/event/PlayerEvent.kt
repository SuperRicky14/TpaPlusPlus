package net.superricky.tpaplusplus.event

import kotlinx.coroutines.launch
import net.minecraft.server.network.ServerPlayerEntity
import net.superricky.tpaplusplus.TpaPlusPlus
import net.superricky.tpaplusplus.database.DatabaseManager

object PlayerEvent {
    fun joinEvent(player: ServerPlayerEntity) {
        TpaPlusPlus.launch {
            player.name.literalString?.let { DatabaseManager.insertPlayer(player.uuid, it) }
        }
    }
}
