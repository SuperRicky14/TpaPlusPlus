package net.superricky.tpaplusplus.command.commands

import kotlinx.coroutines.launch
import net.minecraft.server.command.CommandManager.literal
import net.minecraft.text.Text
import net.superricky.tpaplusplus.TpaPlusPlus
import net.superricky.tpaplusplus.async.*
import net.superricky.tpaplusplus.command.BuildableCommand
import net.superricky.tpaplusplus.command.CommandResult
import net.superricky.tpaplusplus.config.config.Config
import net.superricky.tpaplusplus.config.config.command.CommandCooldownSpec
import net.superricky.tpaplusplus.config.config.command.CommandDelaySpec
import net.superricky.tpaplusplus.config.config.command.CommandDistanceSpec
import net.superricky.tpaplusplus.config.config.command.CommandNameSpec
import net.superricky.tpaplusplus.utility.*

object ToggleCommand : AsyncCommand(), BuildableCommand {
    init {
        commandName = Config.getConfig()[CommandNameSpec.tpatoggleCommand]
    }

    override fun build(): LiteralNode =
        literal(commandName)
            .then(
                literal("on")
                    .executes { switchToggle(it, true) }
            )
            .then(
                literal("off")
                    .executes { switchToggle(it, false) }
            )
            .executes { switchToggle(it) }
            .build()

    override fun getCooldownTime(): Double = Config.getConfig()[CommandCooldownSpec.toggleCooldown]

    override fun getDelayTime(): Double = Config.getConfig()[CommandDelaySpec.toggleDelay]

    override fun getMinDistance(): Double = Config.getConfig()[CommandDistanceSpec.toggleDistance]

    private fun switchToggle(context: Context): Int {
        val source = context.source
        val sender = source.player
        sender ?: return CommandResult.SENDER_NOT_EXIST.status
        AsyncCommandHelper.schedule(
            AsyncCommandData(
                AsyncRequest(AsyncCommandType.TOGGLE, sender),
                LevelBoundVec3(sender.getDimension(), sender.pos),
                AsyncCommandEventFactory
                    .addListener(AsyncCommandEvent.REQUEST_AFTER_DELAY) {
                        TpaPlusPlus.launch {
                            val blocked = TpaPlusPlus.dataService.playerSwitchToggle(sender.uuid)
                            if (blocked) {
                                sender.sendMessage(
                                    Text.translatable("command.toggle.success.on").setStyle(TextColorPallet.primary)
                                )
                            } else {
                                sender.sendMessage(
                                    Text.translatable("command.toggle.success.off").setStyle(TextColorPallet.primary)
                                )
                            }
                            AsyncCommandHelper.addCooldown(sender.uuid, AsyncCommandType.TOGGLE)
                        }
                    }
            )
        )
        return CommandResult.NORMAL.status
    }

    private fun switchToggle(context: Context, blocked: Boolean): Int {
        val source = context.source
        val sender = source.player
        sender ?: return CommandResult.SENDER_NOT_EXIST.status
        AsyncCommandHelper.schedule(
            AsyncCommandData(
                AsyncRequest(AsyncCommandType.TOGGLE, sender),
                LevelBoundVec3(sender.getDimension(), sender.pos),
                AsyncCommandEventFactory
                    .addListener(AsyncCommandEvent.REQUEST_AFTER_DELAY) {
                        TpaPlusPlus.launch {
                            if (blocked) {
                                sender.toggleOn()
                                sender.sendMessage(
                                    Text.translatable("command.toggle.success.on").setStyle(TextColorPallet.primary)
                                )
                            } else {
                                sender.toggleOff()
                                sender.sendMessage(
                                    Text.translatable("command.toggle.success.off").setStyle(TextColorPallet.primary)
                                )
                            }
                            AsyncCommandHelper.addCooldown(sender.uuid, AsyncCommandType.TOGGLE)
                        }
                    }
            )
        )
        return CommandResult.NORMAL.status
    }
}
