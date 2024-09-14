package net.superricky.tpaplusplus.request

import net.minecraft.server.network.ServerPlayerEntity
import net.superricky.tpaplusplus.utility.CommandType

data class Request(
    val sender: ServerPlayerEntity,
    val receiver: ServerPlayerEntity?,
    val commandType: CommandType,
    val hereRequest: Boolean
) {
    override fun toString(): String =
        "Request{sender=${sender.name}, receiver=${receiver?.name}, commandType=$commandType, hereRequest=$hereRequest}"
}
