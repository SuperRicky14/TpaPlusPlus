package net.superricky.tpaplusplus.command.commands

import net.minecraft.server.command.CommandManager.literal
import net.superricky.tpaplusplus.command.BuildableCommand
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.command.CommandNameSpec
import net.superricky.tpaplusplus.utility.LiteralNode

object ToggleCommand : BuildableCommand {
    override fun build(): LiteralNode =
        literal(Config.getConfig()[CommandNameSpec.tpatoggleCommand])
            .build()
}
