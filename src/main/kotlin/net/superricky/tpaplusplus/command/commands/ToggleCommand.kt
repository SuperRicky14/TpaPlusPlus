package net.superricky.tpaplusplus.command.commands

import kotlinx.coroutines.launch
import net.minecraft.server.command.CommandManager.literal
import net.minecraft.text.Text
import net.superricky.tpaplusplus.TpaPlusPlus
import net.superricky.tpaplusplus.command.BuildableCommand
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.command.CommandNameSpec
import net.superricky.tpaplusplus.database.DatabaseManager
import net.superricky.tpaplusplus.utility.*

object ToggleCommand : BuildableCommand {
    override fun build(): LiteralNode =
        literal(Config.getConfig()[CommandNameSpec.tpatoggleCommand])
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

    private fun switchToggle(context: Context): Int {
        TpaPlusPlus.launch {
            val source = context.source
            val sender = source.player
            if (sender == null) {
                source.sendError(Text.translatable("command.toggle.error.sender.not_exist"))
                return@launch
            }
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
        TpaPlusPlus.launch {
            val source = context.source
            val sender = source.player
            if (sender == null) {
                source.sendError(Text.translatable("command.toggle.error.sender.not_exist"))
                return@launch
            }
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
