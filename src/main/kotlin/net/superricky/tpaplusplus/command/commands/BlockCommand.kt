package net.superricky.tpaplusplus.command.commands

import net.minecraft.server.command.CommandManager.literal
import net.superricky.tpaplusplus.command.BuildableCommand
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.command.CommandNameSpec
import net.superricky.tpaplusplus.utility.LiteralNode

object BlockCommand : BuildableCommand {
    override fun build(): LiteralNode =
        literal(Config.getConfig()[CommandNameSpec.tpablockCommand])
            .build()
}
