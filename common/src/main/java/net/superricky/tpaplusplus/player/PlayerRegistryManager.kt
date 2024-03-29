package net.superricky.tpaplusplus.player

import dev.architectury.event.EventResult
import net.minecraft.server.level.ServerPlayer
import java.util.*
import java.util.concurrent.ConcurrentHashMap

private val playerRegistry: MutableMap<UUID, ServerPlayer> = ConcurrentHashMap()

fun onPlayerJoin(joinedPlayer: ServerPlayer): EventResult {
    playerRegistry[joinedPlayer.uuid] = joinedPlayer
    return EventResult.pass()
}

fun onPlayerQuit(playerQuit: ServerPlayer): EventResult {
    playerRegistry.remove(playerQuit.uuid)
    return EventResult.pass()
}

fun getPlayerByUUID(playerUUID: UUID): ServerPlayer? {
    return playerRegistry[playerUUID]
}