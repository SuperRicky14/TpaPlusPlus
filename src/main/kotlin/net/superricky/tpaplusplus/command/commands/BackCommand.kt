package net.superricky.tpaplusplus.command.commands

import net.minecraft.server.command.CommandManager.literal
import net.superricky.tpaplusplus.async.AsyncCommand
import net.superricky.tpaplusplus.async.AsyncCommandData
import net.superricky.tpaplusplus.command.BuildableCommand
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.command.CommandCooldownSpec
import net.superricky.tpaplusplus.config.command.CommandDelaySpec
import net.superricky.tpaplusplus.config.command.CommandDistanceSpec
import net.superricky.tpaplusplus.config.command.CommandNameSpec
import net.superricky.tpaplusplus.utility.LiteralNode

object BackCommand : AsyncCommand(), BuildableCommand {
    init {
        commandName = Config.getConfig()[CommandNameSpec.backCommand]
    }

    override fun build(): LiteralNode =
        literal(commandName)
            .build()

    override fun getCooldownTime(): Double = Config.getConfig()[CommandCooldownSpec.backCooldown]

    override fun getDelayTime(): Double = Config.getConfig()[CommandDelaySpec.backDelay]

    override fun checkWindupDistance(asyncCommandData: AsyncCommandData): Boolean =
        checkWindupDistance(
            asyncCommandData,
            ::getSenderDistance,
            Config.getConfig()[CommandDistanceSpec.backDistance]
        )
}
