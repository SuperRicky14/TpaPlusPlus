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
                    .executes { tpaRequest(it) }
            )
            .build()

    override fun getCooldownTime(): Double = Config.getConfig()[CommandCooldownSpec.tpaCooldown]

    override fun getDelayTime(): Double = Config.getConfig()[CommandDelaySpec.tpaDelay]

    override fun getMinDistance(): Double = Config.getConfig()[CommandDistanceSpec.tpaDistance]

    private fun tpaRequest(context: Context): Int {
        val (result, sender, receiver) = checkSenderReceiver(context)
        if (result != CommandResult.NORMAL) return result.status
        sender!!
        receiver!!
        if (CommandHelper.checkToggled(sender, receiver) ||
            CommandHelper.checkBlocked(sender, receiver) ||
            CommandHelper.checkRequestExist(sender, receiver, AsyncCommandType.TPA)
        ) {
            return CommandResult.NORMAL.status
        }
        LimitationHelper.checkLimitation(sender, receiver)?.let {
            sender.sendMessage(it)
            return CommandResult.NORMAL.status
        }
        AsyncCommandHelper.schedule(
            AsyncCommandData(
                AsyncRequest(sender, receiver, AsyncCommandType.TPA, sender, receiver),
                LevelBoundVec3(sender.getDimension(), sender.pos),
                AsyncCommandEventFactory
                    .addListener(AsyncCommandEvent.REQUEST_AFTER_DELAY) {
                        sender.sendMessageWithPlayerName("command.tpa.request.sender", receiver)
                        receiver.sendMessageWithPlayerName("command.tpa.request.receiver", sender)
                    }
                    .addListener(AsyncCommandEvent.REQUEST_TIMEOUT) {
                        sender.sendMessageWithPlayerName("command.tpa.timeout.sender", receiver)
                        receiver.sendMessageWithPlayerName("command.tpa.timeout.receiver", sender)
                    }
                    .addListener(AsyncCommandEvent.REQUEST_ACCEPTED) {
                        sender.sendMessageWithPlayerName("command.tpa.request.accept.sender", receiver)
                        receiver.sendMessageWithPlayerName("command.tpa.request.accept.receiver", sender)
                        AsyncCommandHelper.teleport(it)
                    }
                    .addListener(AsyncCommandEvent.REQUEST_CANCELED) {
                        sender.sendMessageWithPlayerName("command.tpa.request.cancel.sender", receiver)
                        receiver.sendMessageWithPlayerName("command.tpa.request.cancel.receiver", sender)
                    }
                    .addListener(AsyncCommandEvent.REQUEST_REFUSED) {
                        sender.sendMessageWithPlayerName("command.tpa.request.refuse.sender", receiver)
                        receiver.sendMessageWithPlayerName("command.tpa.request.refuse.receiver", sender)
                    }
                    .addListener(AsyncCommandEvent.TELEPORT_OUT_DISTANCE) {
                        sender.sendMessage(
                            Text.translatable("command.teleport.out_distance").setStyle(TextColorPallet.error)
                        )
                    }
                    .addListener(AsyncCommandEvent.TELEPORT_UPDATE_MESSAGE) {
                        sender.sendTeleportTime(it.getRequest().delay)
                    }
            )
        )
        return CommandResult.NORMAL.status
    }
}
