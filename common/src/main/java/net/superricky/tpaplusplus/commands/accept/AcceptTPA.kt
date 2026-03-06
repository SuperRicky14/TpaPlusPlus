package net.superricky.tpaplusplus.commands.accept

import net.minecraft.network.chat.Component
import net.minecraft.server.level.ServerPlayer
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.Messages
import net.superricky.tpaplusplus.cooldown.CommandType
import net.superricky.tpaplusplus.cooldown.CooldownData
import net.superricky.tpaplusplus.cooldown.CooldownManager
import net.superricky.tpaplusplus.cooldown.CooldownManager.getPlayerCooldown
import net.superricky.tpaplusplus.cooldown.CooldownManager.scheduleCooldown
import net.superricky.tpaplusplus.requests.Request
import net.superricky.tpaplusplus.requests.RequestGrabUtil.getReceiverRequest
import net.superricky.tpaplusplus.requests.RequestHelper.requestSet
import net.superricky.tpaplusplus.requests.RequestHelper.teleport
import net.superricky.tpaplusplus.util.template
import java.time.Duration
import java.util.*
import java.util.Map

object AcceptTPA {
    // Accept command is run by the sender, hence why it's in the sender's point of view.
    fun acceptFunctionality(request: Request?, receiver: ServerPlayer) {
        if (request == null) {
            receiver.sendSystemMessage(Component.literal(Messages.ERR_REQUEST_NOT_FOUND.get()))
            return
        }

        val cooldown: CooldownData?
        if ((getPlayerCooldown(receiver.getUUID(), CommandType.ACCEPT).also { cooldown = it }) != null) {
            CooldownManager.notifyCooldown(receiver, cooldown!!)
            return
        }

        if (Config.ACCEPT_COOLDOWN.get() > 0)  // Check if cooldown is enabled
            scheduleCooldown(
                receiver.getUUID(),
                Duration.ofSeconds(Config.ACCEPT_COOLDOWN.get().toLong()),
                CommandType.ACCEPT
            )

        absoluteAcceptFunctionality(request, receiver)
    }

    fun absoluteAcceptFunctionality(request: Request, receiver: ServerPlayer) {
        receiver.sendSystemMessage(
            Component.literal(
                Messages.RECEIVER_ACCEPTS_TPA.get()
                    .template(mapOf("senders_name" to request.sender.name.string))
            )
        )
        request.sender.sendSystemMessage(
            Component.literal(
                Messages.SENDER_GOT_ACCEPTED_TPA.get()
                    .template(mapOf("receivers_name" to request.receiver.name.string))
            )
        )

        teleport(request)

        requestSet.remove(request)
    }

    @JvmStatic
    fun acceptTeleportRequest(receiver: ServerPlayer) {
        val request = getReceiverRequest(receiver)
        acceptFunctionality(request, receiver)
    }

    @JvmStatic
    fun acceptTeleportRequest(receiver: ServerPlayer, sender: ServerPlayer) {
        val request = getReceiverRequest(receiver, sender)
        acceptFunctionality(request, receiver)
    }
}
