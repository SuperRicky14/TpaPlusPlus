package net.superricky.tpaplusplus.command.commands

import kotlinx.coroutines.launch
import net.minecraft.command.argument.EntityArgumentType
import net.minecraft.server.command.CommandManager.argument
import net.minecraft.server.command.CommandManager.literal
import net.minecraft.text.Text
import net.superricky.tpaplusplus.TpaPlusPlus
import net.superricky.tpaplusplus.command.BuildableCommand
import net.superricky.tpaplusplus.config.CommonSpec
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.command.CommandNameSpec
import net.superricky.tpaplusplus.database.DatabaseManager
import net.superricky.tpaplusplus.utility.*

object BlockCommand : BuildableCommand {
    override fun build(): LiteralNode =
        literal(Config.getConfig()[CommandNameSpec.tpablockCommand])
            .then(
                argument("player", EntityArgumentType.player())
                    .executes { blockPlayer(it) }
            )
            .build()

    private fun blockPlayer(context: Context): Int {
        val source = context.source
        val sender = source.player
        val target = EntityArgumentType.getPlayer(context, "player")
        if (sender == null) {
            source.sendError(Text.translatable("command.error.sender.not_exist"))
            return CommandResult.SENDER_NOT_EXIST.status
        }
        if (target == null) {
            source.sendError(Text.translatable("command.error.target.not_exist"))
            return CommandResult.TARGET_NOT_EXIST.status
        }
        if (sender == target) {
            source.sendError(Text.translatable("command.block.error.self"))
            return CommandResult.SELF_CHECK_ERROR.status
        }
        TpaPlusPlus.launch {
            if (DatabaseManager.insertBlockedPlayer(sender.uuid, target.uuid)) {
                source.sendFeedback(
                    {
                        Text.translatable(
                            "command.block.success",
                            target.getColoredName(TextColorPallet.secondary)
                        )
                            .setStyle(TextColorPallet.primary)
                    },
                    false
                )
                if (Config.getConfig()[CommonSpec.showBlockedMessage]) {
                    target.sendMessage(
                        Text.translatable(
                            "command.block.be_blocked",
                            sender.getColoredName(TextColorPallet.secondary)
                        ).setStyle(TextColorPallet.primary)
                    )
                }
            } else {
                source.sendFeedback(
                    {
                        Text.translatable(
                            "command.block.fail",
                            target.getColoredName(TextColorPallet.secondary)
                        )
                            .setStyle(TextColorPallet.primary)
                    },
                    false
                )
            }
        }
        return CommandResult.NORMAL.status
    }
}
