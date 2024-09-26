package net.superricky.tpaplusplus.command.commands

import net.minecraft.server.command.CommandManager.literal
import net.minecraft.text.Text
import net.superricky.tpaplusplus.async.*
import net.superricky.tpaplusplus.command.BuildableCommand
import net.superricky.tpaplusplus.command.CommandHelper
import net.superricky.tpaplusplus.command.CommandHelper.checkSenderReceiver
import net.superricky.tpaplusplus.command.CommandResult
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.command.CommandCooldownSpec
import net.superricky.tpaplusplus.config.command.CommandDelaySpec
import net.superricky.tpaplusplus.config.command.CommandDistanceSpec
import net.superricky.tpaplusplus.config.command.CommandNameSpec
import net.superricky.tpaplusplus.utility.*

object TpaHereCommand : AsyncCommand(), BuildableCommand {
    init {
        commandName = Config.getConfig()[CommandNameSpec.tpahereCommand]
    }

    override fun build(): LiteralNode =
        literal(commandName)
            .executes { tpaHereRequest(it) }
            .build()

    override fun getCooldownTime(): Double = Config.getConfig()[CommandCooldownSpec.tpahereCooldown]
    override fun getDelayTime(): Double = Config.getConfig()[CommandDelaySpec.tpahereDelay]

    override fun getMinDistance(): Double = Config.getConfig()[CommandDistanceSpec.tpahereDistance]

    private fun asyncCommandCallback(event: AsyncCommandEvent, asyncCommandData: AsyncCommandData) {
        val asyncRequest = asyncCommandData.getRequest()
        asyncRequest.receiver!!
        when (event) {
            AsyncCommandEvent.REQUEST_AFTER_DELAY -> {
                asyncRequest.sender.sendMessageWithPlayerName("command.tpahere.request.sender", asyncRequest.receiver)
                asyncRequest.receiver.sendMessageWithPlayerName("command.tpahere.request.receiver", asyncRequest.sender)
            }

            AsyncCommandEvent.REQUEST_TIMEOUT -> {
                asyncRequest.sender.sendMessageWithPlayerName("command.tpahere.timeout.sender", asyncRequest.receiver)
                asyncRequest.receiver.sendMessageWithPlayerName("command.tpahere.timeout.receiver", asyncRequest.sender)
            }

            AsyncCommandEvent.REQUEST_ACCEPTED -> {
                require(asyncRequest.canBeTeleported()) { "Request can't be teleported!" }
                asyncRequest.sender.sendMessageWithPlayerName(
                    "command.tpahere.request.accept.sender",
                    asyncRequest.receiver
                )
                asyncRequest.receiver.sendMessageWithPlayerName(
                    "command.tpahere.request.accept.receiver",
                    asyncRequest.sender
                )
                AsyncCommandHelper.teleport(asyncCommandData)
            }

            AsyncCommandEvent.REQUEST_CANCELED -> {
                asyncRequest.sender.sendMessageWithPlayerName(
                    "command.tpahere.request.cancel.sender",
                    asyncRequest.receiver
                )
                asyncRequest.receiver.sendMessageWithPlayerName(
                    "command.tpahere.request.cancel.receiver",
                    asyncRequest.sender
                )
            }

            AsyncCommandEvent.REQUEST_REFUSED -> {
                asyncRequest.sender.sendMessageWithPlayerName(
                    "command.tpahere.request.refuse.sender",
                    asyncRequest.receiver
                )
                asyncRequest.receiver.sendMessageWithPlayerName(
                    "command.tpahere.request.refuse.receiver",
                    asyncRequest.sender
                )
            }

            AsyncCommandEvent.TELEPORT_OUT_DISTANCE -> {
                asyncRequest.from?.sendMessage(
                    Text.translatable("command.teleport.out_distance").setStyle(TextColorPallet.error)
                )
            }

            AsyncCommandEvent.TELEPORT_UPDATE_MESSAGE -> {
                asyncRequest.from?.sendTeleportTime(asyncRequest.delay)
            }

            else -> {}
        }
    }

    private fun tpaHereRequest(context: Context): Int {
        val (result, sender, target) = checkSenderReceiver(context)
        if (result != CommandResult.NORMAL) return result.status
        sender!!
        target!!
        if (CommandHelper.checkToggled(sender, target) ||
            CommandHelper.checkBlocked(sender, target) ||
            CommandHelper.checkRequestExist(sender, target, AsyncCommandType.TPAHERE)
        ) {
            return CommandResult.NORMAL.status
        }
        LimitationHelper.checkLimitation(sender, target)?.let {
            sender.sendMessage(it)
            return CommandResult.NORMAL.status
        }
        AsyncCommandHelper.schedule(
            AsyncCommandData(
                AsyncRequest(sender, target, AsyncCommandType.TPAHERE, target, sender),
                LevelBoundVec3(sender.getDimension(), sender.pos),
                ::asyncCommandCallback
            )
        )
        return CommandResult.NORMAL.status
    }
}
