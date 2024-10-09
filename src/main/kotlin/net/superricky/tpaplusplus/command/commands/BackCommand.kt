package net.superricky.tpaplusplus.command.commands

import net.minecraft.server.command.CommandManager.literal
import net.superricky.tpaplusplus.TpaPlusPlus
import net.superricky.tpaplusplus.async.*
import net.superricky.tpaplusplus.command.BuildableCommand
import net.superricky.tpaplusplus.command.CommandResult
import net.superricky.tpaplusplus.config.config.Config.get
import net.superricky.tpaplusplus.config.config.command.CommandCooldownSpec
import net.superricky.tpaplusplus.config.config.command.CommandDelaySpec
import net.superricky.tpaplusplus.config.config.command.CommandDistanceSpec
import net.superricky.tpaplusplus.config.config.command.CommandNameSpec
import net.superricky.tpaplusplus.config.language.LanguageConfig.getMutableText
import net.superricky.tpaplusplus.config.language.command.BackSpec
import net.superricky.tpaplusplus.config.language.error.ErrorSpec
import net.superricky.tpaplusplus.utility.*

object BackCommand : AsyncCommand(), BuildableCommand {
    init {
        commandName = CommandNameSpec.backCommand.get()
    }

    override fun build(): LiteralNode =
        literal(commandName)
            .executes { backRequest(it) }
            .build()

    override fun getCooldownTime(): Double = CommandCooldownSpec.backCooldown.get()

    override fun getDelayTime(): Double = CommandDelaySpec.backDelay.get()

    override fun getMinDistance(): Double = CommandDistanceSpec.backDistance.get()

    private fun backRequest(context: Context): Int {
        val source = context.source
        val sender = source.player
        sender ?: return CommandResult.SENDER_NOT_EXIST.status
        val playerData = TpaPlusPlus.dataService.getPlayerData(sender)
        val lastDeathPos = playerData.lastDeathPos
        if (lastDeathPos.backed) {
            sender.sendMessage(
                BackSpec.deathNotFound.getMutableText().setStyle(TextColorPallet.error)
            )
            return CommandResult.NORMAL.status
        }
        if (!LimitationHelper.checkDimensionLimitation(sender, lastDeathPos.world.getWorld())) {
            sender.sendMessage(
                ErrorSpec.crossDim.getMutableText(
                    sender.getDimension().value.toString().literal().setStyle(TextColorPallet.errorVariant),
                    lastDeathPos.world.getWorld().value.toString().literal().setStyle(TextColorPallet.errorVariant)
                ).setStyle(TextColorPallet.error)
            )
            return CommandResult.NORMAL.status
        }
        AsyncCommandHelper.schedule(
            AsyncCommandData(
                AsyncRequest(AsyncCommandType.BACK, sender, null, sender),
                LevelBoundVec3(sender.getDimension(), sender.pos),
                AsyncCommandEventFactory
                    .addListener(AsyncCommandEvent.REQUEST_AFTER_DELAY) {
                        sender.sendMessage(
                            BackSpec.teleporting.getMutableText().setStyle(TextColorPallet.primary)
                        )
                        AsyncCommandHelper.teleport(it)
                    }
            )
        )
        return CommandResult.NORMAL.status
    }
}
