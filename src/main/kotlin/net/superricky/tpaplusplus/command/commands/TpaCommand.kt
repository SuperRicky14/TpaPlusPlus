package net.superricky.tpaplusplus.command.commands

import net.minecraft.command.argument.EntityArgumentType
import net.minecraft.server.command.CommandManager.argument
import net.minecraft.server.command.CommandManager.literal
import net.superricky.tpaplusplus.async.*
import net.superricky.tpaplusplus.command.BuildableCommand
import net.superricky.tpaplusplus.command.CommandHelper
import net.superricky.tpaplusplus.command.CommandHelper.checkSenderReceiver
import net.superricky.tpaplusplus.command.CommandResult
import net.superricky.tpaplusplus.config.config.Config.get
import net.superricky.tpaplusplus.config.config.command.CommandCooldownSpec
import net.superricky.tpaplusplus.config.config.command.CommandDelaySpec
import net.superricky.tpaplusplus.config.config.command.CommandDistanceSpec
import net.superricky.tpaplusplus.config.config.command.CommandNameSpec
import net.superricky.tpaplusplus.config.language.command.TpaSpec
import net.superricky.tpaplusplus.utility.*

object TpaCommand : AsyncCommand(), BuildableCommand {
    init {
        commandName = CommandNameSpec.tpaCommand.get()
    }

    override fun build(): LiteralNode =
        literal(commandName)
            .then(
                argument("player", EntityArgumentType.player())
                    .executes { tpaRequest(it) }
            )
            .build()

    override fun getCooldownTime(): Double = CommandCooldownSpec.tpaCooldown.get()

    override fun getDelayTime(): Double = CommandDelaySpec.tpaDelay.get()

    override fun getMinDistance(): Double = CommandDistanceSpec.tpaDistance.get()

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
                AsyncRequest(AsyncCommandType.TPA, sender, receiver, sender, receiver),
                LevelBoundVec3(sender.getDimension(), sender.pos),
                AsyncCommandEventFactory
                    .addListener(AsyncCommandEvent.REQUEST_AFTER_DELAY) {
                        sender.sendMessageWithPlayerName(TpaSpec.requestSender, receiver)
                        receiver.sendMessageWithPlayerName(TpaSpec.requestReceiver, sender)
                    }
                    .addListener(AsyncCommandEvent.REQUEST_TIMEOUT) {
                        sender.sendMessageWithPlayerName(TpaSpec.timeoutSender, receiver)
                        receiver.sendMessageWithPlayerName(TpaSpec.timeoutReceiver, sender)
                    }
                    .addListener(AsyncCommandEvent.REQUEST_ACCEPTED) {
                        sender.sendMessageWithPlayerName(TpaSpec.acceptSender, receiver)
                        receiver.sendMessageWithPlayerName(TpaSpec.acceptReceiver, sender)
                        AsyncCommandHelper.teleport(it)
                    }
                    .addListener(AsyncCommandEvent.REQUEST_CANCELED) {
                        sender.sendMessageWithPlayerName(TpaSpec.cancelSender, receiver)
                        receiver.sendMessageWithPlayerName(TpaSpec.cancelReceiver, sender)
                    }
                    .addListener(AsyncCommandEvent.REQUEST_REFUSED) {
                        sender.sendMessageWithPlayerName(TpaSpec.refuseSender, receiver)
                        receiver.sendMessageWithPlayerName(TpaSpec.refuseReceiver, sender)
                    }
            )
        )
        return CommandResult.NORMAL.status
    }
}
