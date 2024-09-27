package net.superricky.tpaplusplus.async

import net.minecraft.server.network.ServerPlayerEntity

/**
 * A class which store command request
 */
class AsyncRequest(
    /**
     * Command type
     * @see AsyncCommandType
     */
    val commandType: AsyncCommandType,
    /**
     * Player who send request, must exist
     */
    val sender: ServerPlayerEntity,
    /**
     * Player who receive request
     * For some command this is null
     */
    val receiver: ServerPlayerEntity? = null,
    /**
     * Player who will be teleported
     * For some command this is null
     */
    val from: ServerPlayerEntity? = null,
    /**
     * Player who is the teleport target
     * For some command this is null
     */
    val to: ServerPlayerEntity? = null
) {
    var delay: Double = commandType.handler.getDelayTime()
    var cooldown: Double = commandType.handler.getCooldownTime()

    /**
     * Check whether this is a teleport request
     * @return True is it is else False
     */
    fun isTeleportRequest(): Boolean = when (commandType) {
        AsyncCommandType.TPA, AsyncCommandType.TPAHERE, AsyncCommandType.BACK -> true
        else -> false
    }

    /**
     * Check whether this teleport request is acceptable
     */
    fun acceptable(): Boolean = isTeleportRequest() && commandType != AsyncCommandType.BACK

    override fun toString(): String =
        "Request{sender=${sender.name}, receiver=${receiver?.name}, from=$from, to=$to, commandType=$commandType"
}
