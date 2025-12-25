package net.superricky.tpaplusplus.windupcooldown.windup

import kotlinx.coroutines.*
import net.minecraft.network.chat.Component
import net.superricky.tpaplusplus.config.Messages
import net.superricky.tpaplusplus.util.MsgFmt

object AsyncWindup {
    private val dispatcher: CoroutineDispatcher = Dispatchers.IO
    private val scope: CoroutineScope = CoroutineScope(dispatcher)

    fun schedule(abstractWindup: AbstractWindup) {
        require(!abstractWindup.cancelled.get()) { "Tried to schedule a windupData that has already been cancelled." }

        // Request is a valid request.
        addWindupData(abstractWindup) // Add it to the tracked windupData.

        scope.launch {
            var countingDown = true
            while (countingDown) {
                countingDown = countdownIteratively(abstractWindup)
                delay(1000L)
            }
        }
    }

    /**
     * @return True if the countdown should be continued for another iteration
     *         False if the countdown should stop
     */
    private fun countdownIteratively(abstractWindup: AbstractWindup): Boolean {
        if (abstractWindup.windupDelay.get() > 0) {
            // Countdown not finished
            if (abstractWindup.cancelled.get()) {
                removeWindupData(abstractWindup)
                return false
            }

            abstractWindup.windupPlayer.sendSystemMessage(Component.literal(MsgFmt.fmt(Messages.WINDUP_TIME_REMAINING.get(),
                mapOf("time" to abstractWindup.windupDelay.toString()))))

            abstractWindup.windupDelay.decrementAndGet()
            return true
        }

        if (abstractWindup.cancelled.get()) {
            removeWindupData(abstractWindup)
            return false
        }

        // Delay is zero, countdown finished
        onCountdownFinished(abstractWindup)

        removeWindupData(abstractWindup)
        return false
    }



    private fun onCountdownFinished(abstractWindup: AbstractWindup) {
        abstractWindup.onWindupFinished()
    }
}