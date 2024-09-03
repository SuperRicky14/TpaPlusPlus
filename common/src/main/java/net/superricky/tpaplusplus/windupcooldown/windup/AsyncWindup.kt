package net.superricky.tpaplusplus.windupcooldown.windup

import kotlinx.coroutines.*
import net.minecraft.network.chat.Component
import net.superricky.tpaplusplus.TPAPlusPlus
import net.superricky.tpaplusplus.commands.accept.AcceptTPA
import net.superricky.tpaplusplus.commands.back.Back
import net.superricky.tpaplusplus.commands.block.BlockPlayer
import net.superricky.tpaplusplus.commands.cancel.CancelTPA
import net.superricky.tpaplusplus.commands.deny.DenyTPA
import net.superricky.tpaplusplus.commands.send.SendTPA
import net.superricky.tpaplusplus.commands.toggle.TPToggle
import net.superricky.tpaplusplus.commands.unblock.UnBlockPlayer
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.Messages
import net.superricky.tpaplusplus.config.formatters.MessageParser
import net.superricky.tpaplusplus.windupcooldown.CommandType
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.util.*

private val dispatcher: CoroutineDispatcher = Dispatchers.IO
private val scope: CoroutineScope = CoroutineScope(dispatcher)

private val LOGGER: Logger = LoggerFactory.getLogger(TPAPlusPlus.MOD_ID)

private fun validateInvalidWindupData(windupData: WindupData) {
    if (AsyncWindupHelper.playersAreNull(*windupData.players)) {
        throw IllegalArgumentException("The playerlist or one of the players inside is null! Please report this issue to the TPA++ issue page immediately.")
    }

    if (windupData.players.isEmpty()) {
        throw IllegalArgumentException("No players were specified when attempting to schedule this task! Please report this issue to the TPA++ issue page immediately.")
    }
}

fun schedule(windupData: WindupData) {
    // Prevent the tasks specified from being in an illegal state (e.g: the caller specified an ACCEPT type, although there was no request to accept).
    try {
        validateInvalidWindupData(windupData)
        AsyncWindupHelper.validateTypeSpecificWindupData(windupData)
    } catch (e: IllegalArgumentException) {
        LOGGER.error(e.message)
        windupData.players[0].sendSystemMessage(Component.literal("§4An internal server error occurred, please check console for more information."))
        return
    }

    require(!windupData.cancelled.get()) { "Tried to schedule a windupData that has already been cancelled." }

    // Request is a valid request.
    addWindupData(windupData) // Add it to the tracked windupData.

    // Decide whether to message the player.
    if (AsyncWindupHelper.extractDecimalPart(windupData.originalDelay) > Config.WINDUP_DECIMAL_MESSAGE_THRESHOLD.get()) {
        // Decimal Number is greater than the threshold specified in the config, so we notify the player here.
        // Message the player the configured WINDUP_TIME_REMAINING message, except since it isn't an integer, we have to round it here to 3 decimal places (which it should be anyway's)
        AsyncWindupHelper.fastMSG(
            MessageParser.enhancedFormatter(
                Messages.WINDUP_TIME_REMAINING.get(),
                java.util.Map.of("time", (Math.round(windupData.originalDelay * 1000).toDouble() / 1000).toString()) as Map<String, Any>?
            ),
            *windupData.players
        )
    }

    scope.launch {
        delay(AsyncWindupHelper.getLongMillisFromDoubleSeconds(AsyncWindupHelper.extractDecimalPart(windupData.originalDelay)))

        var countingDown = true
        while (countingDown) {
            countingDown = countdownIteratively(windupData)
            delay(1000L)
        }
    }
}

/**
 * @return True if the countdown should be continued for another iteration
 *         False if the countdown should stop
 */
private fun countdownIteratively(windupData: WindupData): Boolean {
    if (windupData.delay.get() > 0) { // Countdown not finished
        if (windupData.cancelled.get()) {
            removeWindupData(windupData)
            return false
        }

        AsyncWindupHelper.fastMSG(
            MessageParser.enhancedFormatter(
                Messages.WINDUP_TIME_REMAINING.get(),
                java.util.Map.of("time", windupData.delay.toString()) as Map<String, Any>?
            ), *windupData.players
        )

        windupData.delay.decrementAndGet()
        return true
    }

    if (windupData.cancelled.get()) {
        removeWindupData(windupData)
        return false
    }

    // Delay is zero, countdown finished
    try {
        onCountdownFinished(windupData)
    } catch (e: NullPointerException) {
        printScaryNullPointerExceptionError(e)
    }

    removeWindupData(windupData)
    return false
}

private fun printScaryNullPointerExceptionError(e: NullPointerException) {
    LOGGER.error(e.message)
    LOGGER.warn("A NullPointerException was caught by TPA++. This is an extremely rare case, and if this does not happen again you should be able to continue playing.")
    LOGGER.warn("Although, please report this issue to TPA++ at \"https://github.com/SuperRicky14/TpaPlusPlus/issues\" along with the error message.")
    LOGGER.warn("If you notice any unexpected behaviour shutdown your server immediately and create a backup of your world before turning it back on.")
}

private fun onCountdownFinished(windupData: WindupData) {
    when (windupData.type) {
        CommandType.BACK -> Back.absoluteTeleportToLatestDeath(windupData.players[0], windupData.deathPosition)

        CommandType.ACCEPT -> AcceptTPA.absoluteAcceptFunctionality(
            Objects.requireNonNull(windupData.request), windupData.request!!.receiver
        )

        CommandType.DENY -> DenyTPA.absoluteDeny(Objects.requireNonNull(windupData.request))

        CommandType.CANCEL -> CancelTPA.absoluteCancel(Objects.requireNonNull(windupData.request))

        CommandType.TPA, CommandType.TPAHERE -> SendTPA.absoluteSendTeleportRequest(
            windupData.players[0],
            windupData.players[1], Objects.requireNonNull(windupData.hereRequest)
        )

        CommandType.BLOCK -> BlockPlayer.absoluteBlockPlayer(
            Objects.requireNonNull(windupData.playerData),
            windupData.players[0], windupData.players[1]
        )

        CommandType.TOGGLE -> TPToggle.toggleTP(windupData.players[0])

        CommandType.UNBLOCK -> UnBlockPlayer.absoluteUnBlockPlayer(
            Objects.requireNonNull(windupData.playerData),
            windupData.players[0], windupData.players[1]
        )
    }
}