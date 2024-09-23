package net.superricky.tpaplusplus.command.commands

import net.minecraft.command.argument.EntityArgumentType
import net.minecraft.server.command.CommandManager.argument
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

object TpaCommand : AsyncCommand(), BuildableCommand {
    init {
        commandName = Config.getConfig()[CommandNameSpec.tpaCommand]
    }

    override fun build(): LiteralNode =
        literal(commandName)
            .then(
                argument("player", EntityArgumentType.player())
                    .executes { tpaPlayer(it) }
            )
            .build()

    override fun getCooldownTime(): Double = Config.getConfig()[CommandCooldownSpec.tpaCooldown]

    override fun getDelayTime(): Double = Config.getConfig()[CommandDelaySpec.tpaDelay]

    override fun checkWindupDistance(asyncCommandData: AsyncCommandData): Boolean =
        checkWindupDistance(
            asyncCommandData,
            ::getSenderDistance,
            Config.getConfig()[CommandDistanceSpec.tpaDistance]
        )

    private fun asyncCommandCallback(result: AsyncCommandEvent, asyncCommandData: AsyncCommandData) {
        val asyncRequest = asyncCommandData.getRequest()
        require(asyncRequest.receiver != null) { "Receiver cannot be null" }
        when (result) {
            AsyncCommandEvent.REQUEST_AFTER_DELAY -> {
                asyncRequest.sender.sendMessageWithPlayerName("command.tpa.request.sender", asyncRequest.receiver)
                asyncRequest.receiver.sendMessageWithPlayerName("command.tpa.request.receiver", asyncRequest.sender)
            }

            AsyncCommandEvent.REQUEST_TIMEOUT -> {
                asyncRequest.sender.sendMessageWithPlayerName("command.tpa.timeout.sender", asyncRequest.receiver)
                asyncRequest.receiver.sendMessageWithPlayerName("command.tpa.timeout.receiver", asyncRequest.sender)
            }

            AsyncCommandEvent.REQUEST_ACCEPTED -> {
                require(asyncRequest.canBeTeleported()) { "Request can't be teleported!" }
                asyncRequest.sender.sendMessageWithPlayerName("command.tpa.request.accept.from", asyncRequest.receiver)
                asyncRequest.receiver.sendMessageWithPlayerName("command.tpa.request.accept.to", asyncRequest.sender)
                AsyncCommandHelper.teleport(asyncCommandData)
                if (AsyncCommandType.TPA.handler.getCooldownTime() != 0.0) {
                    AsyncCommandHelper.addCooldown(asyncRequest.sender.uuid, AsyncCommandType.TPA)
                }
                if (AsyncCommandType.ACCEPT.handler.getCooldownTime() != 0.0) {
                    AsyncCommandHelper.addCooldown(asyncRequest.receiver.uuid, AsyncCommandType.ACCEPT)
                }
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

    private fun tpaPlayer(context: Context): Int {
        val (result, sender, target) = checkSenderReceiver(context)
        if (result != CommandResult.NORMAL) return result.status
        sender!!
        target!!
        if (CommandHelper.checkToggled(sender, target) ||
            CommandHelper.checkBlocked(sender, target) ||
            CommandHelper.checkRequestExist(sender, target, AsyncCommandType.TPA)
        ) {
            return CommandResult.NORMAL.status
        }
        LimitationHelper.checkLimitation(sender, target)?.let {
            sender.sendMessage(it)
            return CommandResult.NORMAL.status
        }
        AsyncCommandHelper.schedule(
            AsyncCommandData(
                AsyncRequest(sender, target, AsyncCommandType.TPA, sender, target),
                LevelBoundVec3(sender.getDimension(), sender.pos),
                ::asyncCommandCallback
            )
        )
        return CommandResult.NORMAL.status
    }
}
