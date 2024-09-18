package net.superricky.tpaplusplus.async.request

import net.minecraft.server.network.ServerPlayerEntity
import net.superricky.tpaplusplus.async.AsyncCommandType

class Request(
    val sender: ServerPlayerEntity,
    val receiver: ServerPlayerEntity?,
    val commandType: AsyncCommandType,
    val hereRequest: Boolean = false
) {
    var delay: Double = commandType.handler.getDelayTime()
    var cooldown: Double = commandType.handler.getCooldownTime()

    override fun toString(): String =
        "Request{sender=${sender.name}, receiver=${receiver?.name}, commandType=$commandType, hereRequest=$hereRequest}"
}
