package net.superricky.tpaplusplus.command.subcommands

import net.minecraft.server.command.CommandManager.literal
import net.superricky.tpaplusplus.GlobalConst.PERMISSION_LEVEL
import net.superricky.tpaplusplus.command.BuildableCommand
import net.superricky.tpaplusplus.command.CommandResult
import net.superricky.tpaplusplus.config.config.CommonSpec
import net.superricky.tpaplusplus.config.config.Config.get
import net.superricky.tpaplusplus.config.language.LanguageConfig
import net.superricky.tpaplusplus.config.language.LanguageConfig.getMutableText
import net.superricky.tpaplusplus.config.language.SystemSpec
import net.superricky.tpaplusplus.utility.Context
import net.superricky.tpaplusplus.utility.LiteralNode
import net.superricky.tpaplusplus.utility.TextColorPallet

object ReloadCommand : BuildableCommand {
    override fun build(): LiteralNode =
        literal("reload")
            .requires {
                it.hasPermissionLevel(PERMISSION_LEVEL)
            }
            .then(
                literal("message")
                    .executes { reloadMessage(it) }
            )
            .executes {
                reloadMessage(it)
            }
            .build()

    private fun reloadMessage(context: Context): Int {
        val source = context.source
        val sender = source.player
        sender ?: return CommandResult.SENDER_NOT_EXIST.status
        sender.sendMessage(SystemSpec.reloadStart.getMutableText().setStyle(TextColorPallet.primary))
        LanguageConfig.loadLangFile(CommonSpec.language.get().lowercase())
        sender.sendMessage(SystemSpec.reloadFinish.getMutableText().setStyle(TextColorPallet.primary))
        return CommandResult.NORMAL.status
    }
}
