package net.superricky.tpaplusplus.command.commands

import net.minecraft.command.argument.EntityArgumentType
import net.minecraft.server.command.CommandManager.argument
import net.minecraft.server.command.CommandManager.literal
import net.superricky.tpaplusplus.async.AsyncCommandData
import net.superricky.tpaplusplus.async.AsyncCommandHelper
import net.superricky.tpaplusplus.command.AsyncCommand
import net.superricky.tpaplusplus.command.BuildableCommand
import net.superricky.tpaplusplus.command.CommandHelper.checkSenderReceiver
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.command.CommandDelaySpec
import net.superricky.tpaplusplus.config.command.CommandDistanceSpec
import net.superricky.tpaplusplus.config.command.CommandNameSpec
import net.superricky.tpaplusplus.request.Request
import net.superricky.tpaplusplus.utility.*

object TpaCommand : BuildableCommand, AsyncCommand {
    override fun build(): LiteralNode =
        literal(Config.getConfig()[CommandNameSpec.tpaCommand])
            .then(
                argument("player", EntityArgumentType.player())
                    .executes { tpaPlayer(it) }
            )
            .build()

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

            AsyncCommandResult.BE_CANCELED -> {
            }

            AsyncCommandResult.OUT_OF_DISTANCE -> {
                request.sender.sendMessage("command.windup.error.out_distance", request.receiver)
                request.receiver.sendMessage("command.tpa.request.cancel", request.sender)
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
            Request(sender, target, CommandType.TPA, false),
            LevelBoundVec3(sender.getDimension(), sender.pos),
            Config.getConfig()[CommandDelaySpec.tpaDelay],
            ::asyncCommandCallback
        )
        AsyncCommandHelper.schedule(asyncCommandData)
        return CommandResult.NORMAL.status
    }
}
