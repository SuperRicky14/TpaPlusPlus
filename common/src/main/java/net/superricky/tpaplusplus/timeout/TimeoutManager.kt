package net.superricky.tpaplusplus.timeout

import dev.architectury.event.EventResult
import kotlinx.coroutines.*
import net.minecraft.network.chat.Component
import net.superricky.tpaplusplus.config.Messages
import net.superricky.tpaplusplus.requests.Request
import net.superricky.tpaplusplus.requests.RequestHelper
import net.superricky.tpaplusplus.util.MsgFmt

// Create a shared thread pool
private val dispatcher: CoroutineDispatcher = Dispatchers.IO
private val scope: CoroutineScope = CoroutineScope(dispatcher)

fun scheduleTeleportTimeout(request: Request, timeoutSeconds: Long) {
    scope.launch {
        delay(timeoutSeconds * 1000L)
        RequestTimeoutEvent.EVENT.invoker().onRequestTimeout(request)
    }
}

fun onTimeoutEvent(request: Request): EventResult {
    // Check if the request still exists before printing a timeout message
    if (!RequestHelper.teleportRequestExists(request)) return EventResult.pass()

    val receiver = request.receiver
    val sender = request.sender

    if (request.isHereRequest) {
        sender.sendSystemMessage(
            Component.literal(
                MsgFmt.fmt(
                    Messages.SENDER_TPAHERE_TIMEOUT.get(),
                    mapOf("receivers_name" to receiver.displayName.string)
                )
            )
        )

        receiver.sendSystemMessage(
            Component.literal(
                MsgFmt.fmt(
                    Messages.RECEIVER_TPAHERE_TIMEOUT.get(),
                    mapOf("senders_name" to sender.displayName.string)
                )
            )
        )

        RequestHelper.getRequestSet().remove(request)
        return EventResult.pass()
    }

    sender.sendSystemMessage(
        Component.literal(
            MsgFmt.fmt(
                Messages.SENDER_TPA_TIMEOUT.get(),
                mapOf("receivers_name" to receiver.displayName.string)
            )
        )
    )

    receiver.sendSystemMessage(
        Component.literal(
            MsgFmt.fmt(
                Messages.RECEIVER_TPA_TIMEOUT.get(),
                mapOf("senders_name" to sender.displayName.string)
            )
        )
    )

    RequestHelper.getRequestSet().remove(request)
    return EventResult.pass()
}