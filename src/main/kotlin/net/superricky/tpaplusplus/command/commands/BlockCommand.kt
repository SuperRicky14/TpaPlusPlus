package net.superricky.tpaplusplus.command.commands

import kotlinx.coroutines.launch
import net.minecraft.command.argument.EntityArgumentType
import net.minecraft.server.command.CommandManager.argument
import net.minecraft.server.command.CommandManager.literal
import net.minecraft.text.Text
import net.superricky.tpaplusplus.TpaPlusPlus
import net.superricky.tpaplusplus.async.AsyncCommandData
import net.superricky.tpaplusplus.command.AsyncCommand
import net.superricky.tpaplusplus.command.BuildableCommand
import net.superricky.tpaplusplus.command.CommandHelper.checkSenderReceiver
import net.superricky.tpaplusplus.config.CommonSpec
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.command.CommandDistanceSpec
import net.superricky.tpaplusplus.config.command.CommandNameSpec
import net.superricky.tpaplusplus.database.DatabaseManager
import net.superricky.tpaplusplus.utility.*

object BlockCommand : BuildableCommand, AsyncCommand {
    override fun build(): LiteralNode =
        literal(Config.getConfig()[CommandNameSpec.tpablockCommand])
            .then(
                argument("player", EntityArgumentType.player())
                    .executes { blockPlayer(it) }
            )
            .build()

    override fun checkWindupDistance(asyncCommandData: AsyncCommandData): Boolean =
        checkWindupDistance(
            asyncCommandData,
            ::getSenderDistance,
            Config.getConfig()[CommandDistanceSpec.blockDistance]
        )

    private fun blockPlayer(context: Context): Int {
        val source = context.source
        val (result, sender, target) = checkSenderReceiver(context)
        if (result != CommandResult.NORMAL) return result.status
        sender!!
        target!!
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
