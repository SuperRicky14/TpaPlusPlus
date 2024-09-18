package net.superricky.tpaplusplus.command.commands

import net.minecraft.command.argument.EntityArgumentType
import net.minecraft.server.command.CommandManager.argument
import net.minecraft.server.command.CommandManager.literal
import net.superricky.tpaplusplus.async.*
import net.superricky.tpaplusplus.async.request.Request
import net.superricky.tpaplusplus.command.BuildableCommand
import net.superricky.tpaplusplus.command.CommandHelper
import net.superricky.tpaplusplus.command.CommandHelper.checkSenderReceiver
import net.superricky.tpaplusplus.command.CommandResult
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.command.CommandCooldownSpec
import net.superricky.tpaplusplus.config.command.CommandDelaySpec
import net.superricky.tpaplusplus.config.command.CommandDistanceSpec
import net.superricky.tpaplusplus.config.command.CommandNameSpec
import net.superricky.tpaplusplus.utility.Context
import net.superricky.tpaplusplus.utility.LevelBoundVec3
import net.superricky.tpaplusplus.utility.LiteralNode
import net.superricky.tpaplusplus.utility.getDimension

object AcceptCommand : BuildableCommand, AsyncCommand {
    override fun build(): LiteralNode =
        literal(Config.getConfig()[CommandNameSpec.tpaacceptCommand])
            .then(
                argument("player", EntityArgumentType.player())
                    .executes { acceptCommandWithTarget(it) }
            )
            .executes { acceptCommand(it) }
            .build()

    override fun getCooldownTime(): Double = Config.getConfig()[CommandCooldownSpec.acceptCooldown]

    override fun getDelayTime(): Double = Config.getConfig()[CommandDelaySpec.acceptDelay]

    override fun checkWindupDistance(asyncCommandData: AsyncCommandData): Boolean =
        checkWindupDistance(
            asyncCommandData,
            ::getSenderDistance,
            Config.getConfig()[CommandDistanceSpec.acceptDistance]
        )

    private fun acceptCommandWithTarget(context: Context): Int {
        fun asyncCommandCallback(result: AsyncCommandResult, request: Request) {
            if (result == AsyncCommandResult.AFTER_DELAY) {
                val sender = request.sender
                val receiver = request.receiver!!
                val acceptResult = AsyncCommandHelper.acceptRequest(sender, receiver)
                if (acceptResult == AsyncCommandResult.REQUEST_NOT_FOUND) {
                    CommandHelper.requestNotFound(sender, receiver)
                }
            }
        }

        val source = context.source
        val (result, sender, receiver) = checkSenderReceiver(context)
        if (result != CommandResult.NORMAL) return result.status
        sender!!
        receiver!!
        val asyncCommandData = AsyncCommandData(
            Request(sender, receiver, AsyncCommandType.ACCEPT),
            LevelBoundVec3(sender.getDimension(), sender.pos),
            ::asyncCommandCallback
        )
        AsyncCommandHelper.schedule(asyncCommandData)
        return CommandResult.NORMAL.status
    }

    private fun acceptCommand(context: Context): Int {
        fun asyncCommandCallback(result: AsyncCommandResult, request: Request) {
            if (result == AsyncCommandResult.AFTER_DELAY) {
                val sender = request.sender
                val acceptResult = AsyncCommandHelper.acceptRequest(sender)
                if (acceptResult == AsyncCommandResult.REQUEST_NOT_FOUND) {
                    CommandHelper.requestNotFound(sender)
                }
            }
        }

        val source = context.source
        val sender = source.player
        sender ?: return CommandResult.SENDER_NOT_EXIST.status
        val asyncCommandData = AsyncCommandData(
            Request(sender, null, AsyncCommandType.ACCEPT),
            LevelBoundVec3(sender.getDimension(), sender.pos),
            ::asyncCommandCallback
        )
        AsyncCommandHelper.schedule(asyncCommandData)
        return CommandResult.NORMAL.status
    }
}
