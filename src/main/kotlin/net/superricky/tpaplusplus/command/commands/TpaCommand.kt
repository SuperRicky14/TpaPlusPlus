package net.superricky.tpaplusplus.command.commands

import net.minecraft.command.argument.EntityArgumentType
import net.minecraft.server.command.CommandManager.argument
import net.minecraft.server.command.CommandManager.literal
import net.minecraft.text.Text
import net.superricky.tpaplusplus.async.*
import net.superricky.tpaplusplus.async.request.Request
import net.superricky.tpaplusplus.async.request.RequestHelper
import net.superricky.tpaplusplus.command.BuildableCommand
import net.superricky.tpaplusplus.command.CommandHelper.checkSenderReceiver
import net.superricky.tpaplusplus.command.CommandResult
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.command.CommandCooldownSpec
import net.superricky.tpaplusplus.config.command.CommandDelaySpec
import net.superricky.tpaplusplus.config.command.CommandDistanceSpec
import net.superricky.tpaplusplus.config.command.CommandNameSpec
import net.superricky.tpaplusplus.utility.*

object TpaCommand : BuildableCommand, AsyncCommand {
    override fun build(): LiteralNode =
        literal(Config.getConfig()[CommandNameSpec.tpaCommand])
            .then(
                argument("player", EntityArgumentType.player())
                    .executes { tpaPlayer(it) }
            )
            .build()

    override fun getCooldownTime(): Double = Config.getConfig()[CommandCooldownSpec.tpaCooldown]

    override fun getDelayTime(): Double = Config.getConfig()[CommandDelaySpec.tpaDelay]

    override fun checkWindupDistance(asyncCommandData: AsyncCommandData): Boolean =
        checkWindupDistance(
            asyncCommandData,
            ::getSenderDistance,
            Config.getConfig()[CommandDistanceSpec.tpaDistance]
        )

    private fun asyncCommandCallback(result: AsyncCommandResult, request: Request) {
        require(request.receiver != null) { "Receiver cannot be null" }
        when (result) {
            AsyncCommandResult.AFTER_DELAY -> {
                request.sender.sendMessage("command.tpa.request.sender", request.receiver)
                request.receiver.sendMessage("command.tpa.request.receiver", request.sender)
            }

            AsyncCommandResult.TIMEOUT -> {
                request.sender.sendMessage("command.tpa.timeout.sender", request.receiver)
                request.receiver.sendMessage("command.tpa.timeout.receiver", request.sender)
            }

            AsyncCommandResult.OUT_OF_DISTANCE -> {
                request.sender.sendMessage(
                    Text.translatable(
                        "command.windup.error.out_distance",
                        Config.getConfig()[CommandNameSpec.tpaCommand].literal().setStyle(TextColorPallet.secondary)
                    ).setStyle(TextColorPallet.primary)
                )
            }

            AsyncCommandResult.UPDATE_DELAY_MESSAGE -> {
                request.sender.sendRemainTime(request.delay)
            }

            AsyncCommandResult.REQUEST_ACCEPTED -> {
                RequestHelper.teleport(request)
                if (AsyncCommandType.TPA.handler.getCooldownTime() != 0.0) {
                    AsyncCommandHelper.addCooldown(request.sender.uuid, AsyncCommandType.TPA)
                }
                if (AsyncCommandType.ACCEPT.handler.getDelayTime() != 0.0) {
                    AsyncCommandHelper.addCooldown(request.receiver.uuid, AsyncCommandType.ACCEPT)
                }
            }

            AsyncCommandResult.UNDER_COOLDOWN -> {
                request.sender.sendCooldownTime(
                    Config.getConfig()[CommandNameSpec.tpaCommand],
                    request.cooldown.translateTickToSecond()
                )
            }

            else -> {}
        }
    }

    private fun tpaPlayer(context: Context): Int {
        val source = context.source
        val (result, sender, target) = checkSenderReceiver(context)
        if (result != CommandResult.NORMAL) return result.status
        sender!!
        target!!
        val asyncCommandData = AsyncCommandData(
            Request(sender, target, AsyncCommandType.TPA),
            LevelBoundVec3(sender.getDimension(), sender.pos),
            ::asyncCommandCallback
        )
        AsyncCommandHelper.schedule(asyncCommandData)
        return CommandResult.NORMAL.status
    }
}
