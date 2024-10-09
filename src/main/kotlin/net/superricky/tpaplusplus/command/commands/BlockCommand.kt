package net.superricky.tpaplusplus.command.commands

import kotlinx.coroutines.launch
import net.minecraft.command.argument.EntityArgumentType
import net.minecraft.server.command.CommandManager.argument
import net.minecraft.server.command.CommandManager.literal
import net.superricky.tpaplusplus.TpaPlusPlus
import net.superricky.tpaplusplus.async.*
import net.superricky.tpaplusplus.command.BuildableCommand
import net.superricky.tpaplusplus.command.CommandHelper.checkSenderReceiver
import net.superricky.tpaplusplus.command.CommandResult
import net.superricky.tpaplusplus.config.config.CommonSpec
import net.superricky.tpaplusplus.config.config.Config.get
import net.superricky.tpaplusplus.config.config.command.CommandCooldownSpec
import net.superricky.tpaplusplus.config.config.command.CommandDelaySpec
import net.superricky.tpaplusplus.config.config.command.CommandDistanceSpec
import net.superricky.tpaplusplus.config.config.command.CommandNameSpec
import net.superricky.tpaplusplus.config.language.command.BlockSpec
import net.superricky.tpaplusplus.utility.*

object BlockCommand : AsyncCommand(), BuildableCommand {
    init {
        commandName = CommandNameSpec.tpablockCommand.get()
    }

    override fun build(): LiteralNode =
        literal(commandName)
            .then(
                argument("player", EntityArgumentType.player())
                    .executes { blockPlayer(it) }
            )
            .build()

    override fun getCooldownTime(): Double = CommandCooldownSpec.blockCooldown.get()

    override fun getDelayTime(): Double = CommandDelaySpec.blockDelay.get()

    override fun getMinDistance(): Double = CommandDistanceSpec.blockDistance.get()

    private fun blockPlayer(context: Context): Int {
        val (result, sender, receiver) = checkSenderReceiver(context)
        if (result != CommandResult.NORMAL) return result.status
        sender!!
        receiver!!
        AsyncCommandHelper.schedule(
            AsyncCommandData(
                AsyncRequest(AsyncCommandType.BLOCK, sender, receiver),
                LevelBoundVec3(sender.getDimension(), sender.pos),
                AsyncCommandEventFactory
                    .addListener(AsyncCommandEvent.REQUEST_AFTER_DELAY) {
                        TpaPlusPlus.launch {
                            if (TpaPlusPlus.dataService.addBlockPlayer(sender.uuid, receiver.uuid)) {
                                sender.sendMessageWithPlayerName(BlockSpec.success, receiver)
                                if (CommonSpec.showBlockedMessage.get()) {
                                    receiver.sendMessageWithPlayerName(BlockSpec.blockedPlayer, sender)
                                }
                            } else {
                                sender.sendMessageWithPlayerName(BlockSpec.failure, receiver)
                            }
                        }
                    }
            )
        )
        return CommandResult.NORMAL.status
    }
}
