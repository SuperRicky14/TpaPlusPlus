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
import net.superricky.tpaplusplus.config.language.command.TpaHereSpec
import net.superricky.tpaplusplus.utility.*

object TpaHereCommand : AsyncCommand(), BuildableCommand {
    init {
        commandName = CommandNameSpec.tpahereCommand.get()
    }

    override fun build(): LiteralNode =
        literal(commandName)
            .then(
                argument("player", EntityArgumentType.player())
                    .executes { tpaHereRequest(it) }
            )
            .build()

    override fun getCooldownTime(): Double = CommandCooldownSpec.tpahereCooldown.get()
    override fun getDelayTime(): Double = CommandDelaySpec.tpahereDelay.get()

    override fun getMinDistance(): Double = CommandDistanceSpec.tpahereDistance.get()

    private fun tpaHereRequest(context: Context): Int {
        val (result, sender, receiver) = checkSenderReceiver(context)
        if (result != CommandResult.NORMAL) return result.status
        sender!!
        receiver!!
        if (CommandHelper.checkToggled(sender, receiver) ||
            CommandHelper.checkBlocked(sender, receiver) ||
            CommandHelper.checkRequestExist(sender, receiver, AsyncCommandType.TPAHERE)
        ) {
            return CommandResult.NORMAL.status
        }
        LimitationHelper.checkLimitation(sender, receiver)?.let {
            sender.sendMessage(it)
            return CommandResult.NORMAL.status
        }
        AsyncCommandHelper.schedule(
            AsyncCommandData(
                AsyncRequest(AsyncCommandType.TPAHERE, sender, receiver, receiver, sender),
                LevelBoundVec3(sender.getDimension(), sender.pos),
                AsyncCommandEventFactory
                    .addListener(AsyncCommandEvent.REQUEST_AFTER_DELAY) {
                        sender.sendMessageWithPlayerName(TpaHereSpec.requestSender, receiver)
                        receiver.sendMessageWithPlayerName(TpaHereSpec.requestReceiver, sender)
                    }
                    .addListener(AsyncCommandEvent.REQUEST_TIMEOUT) {
                        sender.sendMessageWithPlayerName(TpaHereSpec.timeoutSender, receiver)
                        receiver.sendMessageWithPlayerName(TpaHereSpec.timeoutReceiver, sender)
                    }
                    .addListener(AsyncCommandEvent.REQUEST_ACCEPTED) {
                        sender.sendMessageWithPlayerName(TpaHereSpec.acceptSender, receiver)
                        receiver.sendMessageWithPlayerName(TpaHereSpec.acceptReceiver, sender)
                        AsyncCommandHelper.teleport(it)
                    }.addListener(AsyncCommandEvent.REQUEST_CANCELED) {
                        sender.sendMessageWithPlayerName(TpaHereSpec.cancelSender, receiver)
                        receiver.sendMessageWithPlayerName(TpaHereSpec.cancelReceiver, sender)
                    }
                    .addListener(AsyncCommandEvent.REQUEST_REFUSED) {
                        sender.sendMessageWithPlayerName(TpaHereSpec.refuseSender, receiver)
                        receiver.sendMessageWithPlayerName(TpaHereSpec.refuseReceiver, sender)
                    }
            )
        )
        return CommandResult.NORMAL.status
    }
}
