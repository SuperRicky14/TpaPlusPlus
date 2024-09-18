package net.superricky.tpaplusplus.command.commands

import kotlinx.coroutines.launch
import net.minecraft.command.argument.EntityArgumentType
import net.minecraft.server.command.CommandManager.argument
import net.minecraft.server.command.CommandManager.literal
import net.minecraft.text.Text
import net.superricky.tpaplusplus.TpaPlusPlus
import net.superricky.tpaplusplus.async.AsyncCommand
import net.superricky.tpaplusplus.async.AsyncCommandData
import net.superricky.tpaplusplus.command.BuildableCommand
import net.superricky.tpaplusplus.command.CommandHelper.checkSenderReceiver
import net.superricky.tpaplusplus.command.CommandResult
import net.superricky.tpaplusplus.config.CommonSpec
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.command.CommandCooldownSpec
import net.superricky.tpaplusplus.config.command.CommandDelaySpec
import net.superricky.tpaplusplus.config.command.CommandDistanceSpec
import net.superricky.tpaplusplus.config.command.CommandNameSpec
import net.superricky.tpaplusplus.database.DatabaseManager
import net.superricky.tpaplusplus.utility.Context
import net.superricky.tpaplusplus.utility.LiteralNode
import net.superricky.tpaplusplus.utility.TextColorPallet
import net.superricky.tpaplusplus.utility.getColoredName

object UnblockCommand : BuildableCommand, AsyncCommand {
    override fun build(): LiteralNode =
        literal(Config.getConfig()[CommandNameSpec.tpaunblockCommand])
            .then(
                argument("player", EntityArgumentType.player())
                    .executes { unBlockPlayer(it) }
            )
            .build()

    override fun getCooldownTime(): Double = Config.getConfig()[CommandCooldownSpec.unblockCooldown]

    override fun getDelayTime(): Double = Config.getConfig()[CommandDelaySpec.unblockDelay]

    override fun checkWindupDistance(asyncCommandData: AsyncCommandData): Boolean =
        checkWindupDistance(
            asyncCommandData,
            ::getSenderDistance,
            Config.getConfig()[CommandDistanceSpec.unblockDistance]
        )

    private fun unBlockPlayer(context: Context): Int {
        val source = context.source
        val (result, sender, target) = checkSenderReceiver(context)
        if (result != CommandResult.NORMAL) return result.status
        sender!!
        target!!
        TpaPlusPlus.launch {
            if (DatabaseManager.deleteBlockedPlayer(sender.uuid, target.uuid)) {
                source.sendFeedback(
                    {
                        Text.translatable(
                            "command.unblock.success",
                            target.getColoredName(TextColorPallet.secondary)
                        )
                            .setStyle(TextColorPallet.primary)
                    },
                    false
                )
                if (Config.getConfig()[CommonSpec.showBlockedMessage]) {
                    target.sendMessage(
                        Text.translatable(
                            "command.unblock.be_unblock",
                            sender.getColoredName(TextColorPallet.secondary)
                        ).setStyle(TextColorPallet.primary)
                    )
                }
            } else {
                source.sendFeedback(
                    {
                        Text.translatable(
                            "command.unblock.fail",
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
