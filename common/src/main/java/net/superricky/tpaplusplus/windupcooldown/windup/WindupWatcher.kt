package net.superricky.tpaplusplus.windupcooldown.windup

import kotlinx.coroutines.*
import net.minecraft.network.chat.Component
import net.superricky.tpaplusplus.TPAPlusPlus
import net.superricky.tpaplusplus.config.Messages
import net.superricky.tpaplusplus.util.MsgFmt
import java.util.concurrent.ConcurrentHashMap

object WindupWatcher {
    private val dispatcher: CoroutineDispatcher = Dispatchers.IO
    private val scope: CoroutineScope = CoroutineScope(dispatcher)

    private val trackedAbstractWindupData: MutableSet<AbstractWindup> = ConcurrentHashMap.newKeySet()

    private const val WINDUP_DISABLED_DISTANCE = -1.0
    private var ticking = true

    fun clearTrackedWindupData() {
        trackedAbstractWindupData.clear()
    }

    fun removeWindupData(abstractWindup: AbstractWindup) {
        trackedAbstractWindupData.remove(abstractWindup)
    }

    fun addWindupData(abstractWindup: AbstractWindup) {
        trackedAbstractWindupData.add(abstractWindup)
    }

    fun startAsyncTickLoop(tickRate: Long) {
        scope.launch {
            while (ticking) {
                runTick()
                delay(1000L / tickRate) // We divide 1000 by the tickRate to simultaneously get the inverse of the tickRate (get the amount of time to delay for) and to convert it to milliseconds.
            }
        }
    }

    fun runTick() {
        for (windupData in trackedAbstractWindupData) {
            val windupDistance = windupData.windupDistance

            if (windupDistance == WINDUP_DISABLED_DISTANCE) continue

            if (windupData.cancelled.get()) continue

            if (TPAPlusPlus.distance3D(
                    windupData.acceptX,
                    windupData.acceptY,
                    windupData.acceptZ,
                    windupData.windupPlayer.x,
                    windupData.windupPlayer.y,
                    windupData.windupPlayer.z
                ) > windupDistance
            ) {
                // Squared distance between the position which they started the countdown, and their current position is larger than the allowed distance set in the Config.
                windupData.cancelled.set(true)

                windupData.windupPlayer.sendSystemMessage(
                    Component.literal(
                        MsgFmt.fmt(
                            Messages.PLAYER_MOVED_DURING_WINDUP.get(),
                            mapOf("command_used" to windupData.commandName)
                        )
                    )
                )

                trackedAbstractWindupData.remove(windupData)
            }
        }
    }
}