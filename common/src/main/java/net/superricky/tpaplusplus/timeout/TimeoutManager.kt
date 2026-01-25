package net.superricky.tpaplusplus.timeout

import dev.architectury.event.EventResult
import net.minecraft.network.chat.Component
import net.minecraft.server.MinecraftServer
import net.superricky.tpaplusplus.config.Messages
import net.superricky.tpaplusplus.requests.Request
import net.superricky.tpaplusplus.requests.RequestHelper
import net.superricky.tpaplusplus.util.template
import java.time.Duration
import java.time.Instant

data class Timeout(val timeoutTimestamp: Instant, val request: Request) // Don't like storing Request references, but legacy architecture said no.

object TimeoutManager {
    private val timeoutList: MutableList<Timeout> = mutableListOf();

    fun scheduleTeleportTimeout(request: Request, timeoutSeconds: Duration) {
        timeoutList.add(Timeout(Instant.now() + timeoutSeconds, request))
    }

    fun onMinecraftServerTick(server: MinecraftServer) {
        timeoutList.removeIf { timeout ->
            val timedOut = Instant.now().isAfter(timeout.timeoutTimestamp)
            if (timedOut) {
                RequestTimeoutEvent.EVENT.invoker().onRequestTimeout(timeout)
            }
            timedOut
        }
    }

    fun onTimeoutEvent(timeout: Timeout): EventResult {
        if (!RequestHelper.teleportRequestExists(timeout.request)) {
            return EventResult.pass();
        }

        val receiver = timeout.request.receiver
        val sender = timeout.request.sender

        if (timeout.request.isHereRequest) {
            sender.sendSystemMessage(
                Component.literal(Messages.SENDER_TPAHERE_TIMEOUT.get().template(
                        mapOf("receivers_name" to receiver.displayName.string)
                    )
                )
            )

            receiver.sendSystemMessage(
                Component.literal(
                        Messages.RECEIVER_TPAHERE_TIMEOUT.get().template(
                        mapOf("senders_name" to sender.displayName.string)
                    )
                )
            )

            RequestHelper.getRequestSet().remove(timeout.request)
            return EventResult.pass()
        }

        sender.sendSystemMessage(
            Component.literal(Messages.SENDER_TPA_TIMEOUT.get().template(
                    mapOf("receivers_name" to receiver.displayName.string)
                )
            )
        )

        receiver.sendSystemMessage(
            Component.literal(Messages.RECEIVER_TPA_TIMEOUT.get().template(
                    mapOf("senders_name" to sender.displayName.string)
                )
            )
        )

        RequestHelper.getRequestSet().remove(timeout.request)
        return EventResult.pass()
    }
}