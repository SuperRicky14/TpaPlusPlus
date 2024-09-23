package net.superricky.tpaplusplus.command.commands

import net.minecraft.command.argument.EntityArgumentType
import net.minecraft.server.command.CommandManager.argument
import net.minecraft.server.command.CommandManager.literal
import net.superricky.tpaplusplus.async.*
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

object DenyCommand : AsyncCommand(), BuildableCommand {
    init {
        commandName = Config.getConfig()[CommandNameSpec.tpadenyCommand]
    }

    override fun build(): LiteralNode =
        literal(commandName)
            .then(
                argument("player", EntityArgumentType.player())
                    .executes { refuseRequestWithTarget(it) }
            )
            .executes { refuseRequest(it) }
            .build()

    override fun getCooldownTime(): Double = Config.getConfig()[CommandCooldownSpec.denyCooldown]

    override fun getDelayTime(): Double = Config.getConfig()[CommandDelaySpec.denyDelay]

    override fun checkWindupDistance(asyncCommandData: AsyncCommandData): Boolean =
        checkWindupDistance(
            asyncCommandData,
            ::getSenderDistance,
            Config.getConfig()[CommandDistanceSpec.denyDistance]
        )

    private fun refuseRequestWithTarget(context: Context): Int {
        fun asyncCommandCallback(result: AsyncCommandEvent, asyncCommandData: AsyncCommandData) {
            val asyncRequest = asyncCommandData.getRequest()
            if (result == AsyncCommandEvent.REQUEST_AFTER_DELAY) {
                val sender = asyncRequest.sender
                val receiver = asyncRequest.receiver!!
                if (AsyncCommandHelper.refuseRequest(sender, receiver) == AsyncCommandEvent.REQUEST_NOT_FOUND) {
                    CommandHelper.requestNotFound(sender, receiver)
                }
            }
        }

        val source = context.source
        val (result, sender, receiver) = checkSenderReceiver(context)
        if (result != CommandResult.NORMAL) return result.status
        sender!!
        receiver!!
        AsyncCommandHelper.schedule(
            AsyncCommandData(
                AsyncRequest(sender, receiver, AsyncCommandType.DENY),
                LevelBoundVec3(sender.getDimension(), sender.pos),
                ::asyncCommandCallback
            )
        )
        return CommandResult.NORMAL.status
    }

    private fun refuseRequest(context: Context): Int {
        fun asyncCommandCallback(result: AsyncCommandEvent, asyncCommandData: AsyncCommandData) {
            val asyncRequest = asyncCommandData.getRequest()
            if (result == AsyncCommandEvent.REQUEST_AFTER_DELAY) {
                val sender = asyncRequest.sender
                if (AsyncCommandHelper.refuseRequest(sender) == AsyncCommandEvent.REQUEST_NOT_FOUND) {
                    CommandHelper.requestNotFound(sender)
                }
            }
        }

        val source = context.source
        val sender = source.player
        sender ?: return CommandResult.SENDER_NOT_EXIST.status
        AsyncCommandHelper.schedule(
            AsyncCommandData(
                AsyncRequest(sender, null, AsyncCommandType.DENY),
                LevelBoundVec3(sender.getDimension(), sender.pos),
                ::asyncCommandCallback
            )
        )
        return CommandResult.NORMAL.status
    }
}
