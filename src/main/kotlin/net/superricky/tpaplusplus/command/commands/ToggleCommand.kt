package net.superricky.tpaplusplus.command.commands

import kotlinx.coroutines.launch
import net.minecraft.server.command.CommandManager.literal
import net.minecraft.text.Text
import net.superricky.tpaplusplus.TpaPlusPlus
import net.superricky.tpaplusplus.async.AsyncCommand
import net.superricky.tpaplusplus.async.AsyncCommandData
import net.superricky.tpaplusplus.command.BuildableCommand
import net.superricky.tpaplusplus.command.CommandResult
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.command.CommandCooldownSpec
import net.superricky.tpaplusplus.config.command.CommandDelaySpec
import net.superricky.tpaplusplus.config.command.CommandDistanceSpec
import net.superricky.tpaplusplus.config.command.CommandNameSpec
import net.superricky.tpaplusplus.database.DatabaseManager
import net.superricky.tpaplusplus.utility.Context
import net.superricky.tpaplusplus.utility.LiteralNode
import net.superricky.tpaplusplus.utility.toggleOff
import net.superricky.tpaplusplus.utility.toggleOn

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

    override fun checkWindupDistance(asyncCommandData: AsyncCommandData): Boolean =
        checkWindupDistance(
            asyncCommandData,
            ::getSenderDistance,
            Config.getConfig()[CommandDistanceSpec.toggleDistance]
        )

    private fun switchToggle(context: Context): Int {
        val source = context.source
        val sender = source.player
        sender ?: return CommandResult.SENDER_NOT_EXIST.status
        TpaPlusPlus.launch {
            val blocked = DatabaseManager.playerSwitchBlock(sender.uuid)
            if (blocked) {
                source.sendFeedback({ Text.translatable("command.toggle.success.on") }, false)
            } else {
                source.sendFeedback({ Text.translatable("command.toggle.success.off") }, false)
            }
        }
        return CommandResult.NORMAL.status
    }

    private fun switchToggle(context: Context, blocked: Boolean): Int {
        val source = context.source
        val sender = source.player
        sender ?: return CommandResult.SENDER_NOT_EXIST.status
        TpaPlusPlus.launch {
            if (blocked) {
                sender.toggleOn()
                source.sendFeedback({ Text.translatable("command.toggle.success.on") }, false)
            } else {
                sender.toggleOff()
                source.sendFeedback({ Text.translatable("command.toggle.success.off") }, false)
            }
        }
        return CommandResult.NORMAL.status
    }
}
