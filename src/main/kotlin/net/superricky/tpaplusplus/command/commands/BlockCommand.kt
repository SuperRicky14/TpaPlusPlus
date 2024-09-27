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
import net.superricky.tpaplusplus.config.CommonSpec
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.command.CommandCooldownSpec
import net.superricky.tpaplusplus.config.command.CommandDelaySpec
import net.superricky.tpaplusplus.config.command.CommandDistanceSpec
import net.superricky.tpaplusplus.config.command.CommandNameSpec
import net.superricky.tpaplusplus.utility.*

object BlockCommand : AsyncCommand(), BuildableCommand {
    init {
        commandName = Config.getConfig()[CommandNameSpec.tpablockCommand]
    }

    override fun build(): LiteralNode =
        literal(commandName)
            .then(
                argument("player", EntityArgumentType.player())
                    .executes { blockPlayer(it) }
            )
            .build()

    override fun getCooldownTime(): Double = Config.getConfig()[CommandCooldownSpec.blockCooldown]

    override fun getDelayTime(): Double = Config.getConfig()[CommandDelaySpec.blockDelay]
    override fun getMinDistance(): Double = Config.getConfig()[CommandDistanceSpec.blockDistance]

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
                                sender.sendMessageWithPlayerName("command.block.success", receiver)
                                if (Config.getConfig()[CommonSpec.showBlockedMessage]) {
                                    receiver.sendMessageWithPlayerName("command.block.be_blocked", sender)
                                }
                            } else {
                                sender.sendMessageWithPlayerName("command.block.fail", receiver)
                            }
                        }
                    }
            )
        )
        return CommandResult.NORMAL.status
    }
}
