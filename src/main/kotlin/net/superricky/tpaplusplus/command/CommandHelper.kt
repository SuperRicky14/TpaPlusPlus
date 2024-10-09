package net.superricky.tpaplusplus.command

import net.minecraft.command.argument.EntityArgumentType
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.text.Text
import net.superricky.tpaplusplus.TpaPlusPlus
import net.superricky.tpaplusplus.async.AsyncCommandHelper
import net.superricky.tpaplusplus.async.AsyncCommandType
import net.superricky.tpaplusplus.config.language.LanguageConfig.getMutableText
import net.superricky.tpaplusplus.config.language.error.ErrorRequestSpec
import net.superricky.tpaplusplus.config.language.error.ErrorSpec
import net.superricky.tpaplusplus.utility.Context
import net.superricky.tpaplusplus.utility.TextColorPallet
import net.superricky.tpaplusplus.utility.sendMessageWithPlayerName

object CommandHelper {

    fun requestNotFound(sender: ServerPlayerEntity) =
        sender.sendMessage(Text.translatable("command.error.request.notfound.all").setStyle(TextColorPallet.error))

    fun requestNotFound(sender: ServerPlayerEntity, receiver: ServerPlayerEntity) =
        sender.sendMessage(
            Text.translatable("command.error.request.notfound.target", receiver.name).setStyle(TextColorPallet.error)
        )

    fun checkSenderReceiver(context: Context): Triple<CommandResult, ServerPlayerEntity?, ServerPlayerEntity?> {
        val source = context.source
        val sender = source.player
        val receiver = EntityArgumentType.getPlayer(context, "player")
        val result = checkSenderReceiver(sender, receiver)
        return when (result) {
            CommandResult.SELF_CHECK_ERROR -> {
                source.sendError(ErrorSpec.selfCommand.getMutableText())
                Triple(CommandResult.SELF_CHECK_ERROR, null, null)
            }

            CommandResult.SENDER_NOT_EXIST -> {
                source.sendError(ErrorSpec.senderNotExist.getMutableText())
                Triple(CommandResult.SENDER_NOT_EXIST, null, null)
            }

            CommandResult.RECEIVER_NOT_EXIST -> {
                source.sendError(ErrorSpec.receiverNotExist.getMutableText())
                Triple(CommandResult.RECEIVER_NOT_EXIST, null, null)
            }

            CommandResult.NORMAL -> {
                Triple(CommandResult.NORMAL, sender, receiver)
            }
        }
    }

    fun checkSenderReceiver(sender: ServerPlayerEntity?, receiver: ServerPlayerEntity?): CommandResult {
        sender ?: return CommandResult.SENDER_NOT_EXIST
        receiver ?: return CommandResult.RECEIVER_NOT_EXIST
        if (sender == receiver) {
            return CommandResult.SELF_CHECK_ERROR
        }
        return CommandResult.NORMAL
    }

    fun checkBlocked(sender: ServerPlayerEntity, receiver: ServerPlayerEntity): Boolean {
        if (TpaPlusPlus.dataService.checkPlayerBlocked(receiver.uuid, sender.uuid)) {
            sender.sendMessageWithPlayerName(ErrorRequestSpec.blockedSender, receiver)
            return true
        }
        if (TpaPlusPlus.dataService.checkPlayerBlocked(sender.uuid, receiver.uuid)) {
            sender.sendMessageWithPlayerName(ErrorRequestSpec.blockedReceiver, receiver)
            return true
        }
        return false
    }

    fun checkToggled(sender: ServerPlayerEntity, receiver: ServerPlayerEntity): Boolean {
        if (TpaPlusPlus.dataService.checkPlayerToggle(sender.uuid)) {
            sender.sendMessage(
                ErrorRequestSpec.toggledSender.getMutableText()
                    .setStyle(TextColorPallet.error)
            )
            return true
        }
        if (TpaPlusPlus.dataService.checkPlayerToggle(receiver.uuid)) {
            sender.sendMessageWithPlayerName(ErrorRequestSpec.toggledReceiver, receiver)
            return true
        }
        return false
    }

    fun checkRequestExist(
        sender: ServerPlayerEntity,
        receiver: ServerPlayerEntity,
        commandType: AsyncCommandType
    ): Boolean {
        if (AsyncCommandHelper.checkRequestExist(sender, receiver, commandType)) {
            sender.sendMessageWithPlayerName(ErrorRequestSpec.requestExist, receiver)
            return true
        }
        return false
    }
}
