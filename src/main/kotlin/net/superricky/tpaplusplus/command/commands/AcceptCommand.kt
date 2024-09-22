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
import net.superricky.tpaplusplus.utility.*

object AcceptCommand : AsyncCommand(), BuildableCommand {
    init {
        commandName = Config.getConfig()[CommandNameSpec.tpacceptCommand]
    }

    override fun build(): LiteralNode =
        literal(commandName)
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
        fun asyncCommandCallback(result: AsyncCommandEvent, asyncCommandData: AsyncCommandData) {
            val asyncRequest = asyncCommandData.getRequest()
            when (result) {
                AsyncCommandEvent.REQUEST_AFTER_DELAY -> {
                    val sender = asyncRequest.sender
                    val receiver = asyncRequest.receiver!!
                    val acceptResult = AsyncCommandHelper.acceptRequest(sender, receiver)
                    if (acceptResult == AsyncCommandEvent.REQUEST_NOT_FOUND) {
                        CommandHelper.requestNotFound(sender, receiver)
                    }
                }

                AsyncCommandEvent.REQUEST_UNDER_COOLDOWN -> {
                    asyncRequest.sender.sendCooldownTime(
                        Config.getConfig()[CommandNameSpec.tpacceptCommand],
                        asyncRequest.cooldown.translateTickToSecond()
                    )
                }

                else -> {}
            }
        }

        val source = context.source
        val (result, sender, receiver) = checkSenderReceiver(context)
        if (result != CommandResult.NORMAL) return result.status
        sender!!
        receiver!!
        val asyncCommandData = AsyncCommandData(
            AsyncRequest(sender, receiver, AsyncCommandType.ACCEPT),
            LevelBoundVec3(sender.getDimension(), sender.pos),
            ::asyncCommandCallback
        )
        AsyncCommandHelper.schedule(asyncCommandData)
        return CommandResult.NORMAL.status
    }

    private fun acceptCommand(context: Context): Int {
        fun asyncCommandCallback(result: AsyncCommandEvent, asyncCommandData: AsyncCommandData) {
            val asyncRequest = asyncCommandData.getRequest()
            when (result) {
                AsyncCommandEvent.REQUEST_AFTER_DELAY -> {
                    val sender = asyncRequest.sender
                    val acceptResult = AsyncCommandHelper.acceptRequest(sender)
                    if (acceptResult == AsyncCommandEvent.REQUEST_NOT_FOUND) {
                        CommandHelper.requestNotFound(sender)
                    }
                }

                AsyncCommandEvent.REQUEST_UNDER_COOLDOWN -> {
                    asyncRequest.sender.sendCooldownTime(
                        Config.getConfig()[CommandNameSpec.tpacceptCommand],
                        asyncRequest.cooldown.translateTickToSecond()
                    )
                }

                else -> {}
            }
        }

        val source = context.source
        val sender = source.player
        sender ?: return CommandResult.SENDER_NOT_EXIST.status
        val asyncCommandData = AsyncCommandData(
            AsyncRequest(sender, null, AsyncCommandType.ACCEPT),
            LevelBoundVec3(sender.getDimension(), sender.pos),
            ::asyncCommandCallback
        )
        AsyncCommandHelper.schedule(asyncCommandData)
        return CommandResult.NORMAL.status
    }
}
