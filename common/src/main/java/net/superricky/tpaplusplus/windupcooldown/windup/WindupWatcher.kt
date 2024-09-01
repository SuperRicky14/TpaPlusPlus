package net.superricky.tpaplusplus.windupcooldown.windup

import kotlinx.coroutines.*
import net.minecraft.network.chat.Component
import net.superricky.tpaplusplus.TPAPlusPlus
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.Messages
import net.superricky.tpaplusplus.config.formatters.MessageParser
import net.superricky.tpaplusplus.windupcooldown.CommandType
import java.util.concurrent.ConcurrentHashMap
import kotlin.math.pow

private val dispatcher: CoroutineDispatcher = Dispatchers.IO
private val scope: CoroutineScope = CoroutineScope(dispatcher)

private val trackedWindupData: MutableSet<WindupData> = ConcurrentHashMap.newKeySet()

private const val WINDUP_DISABLED_DISTANCE = -1.0
private var ticking = true

fun clearTrackedWindupData() {
    trackedWindupData.clear()
}

fun removeWindupData(windupData: WindupData) {
    trackedWindupData.remove(windupData)
}

fun addWindupData(windupData: WindupData) {
    trackedWindupData.add(windupData)
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
    for (windupData in trackedWindupData) {
        val windupDistance = getWindupDistance(windupData)

        if (windupDistance == WINDUP_DISABLED_DISTANCE) continue

        if (windupData.cancelled.get()) continue

        if (TPAPlusPlus.noSqrtDistance3D(
                windupData.acceptX,
                windupData.acceptY,
                windupData.acceptZ,
                windupData.players[0].x,
                windupData.players[0].y,
                windupData.players[0].z
            ) > windupDistance.pow(2.0)
        ) {
            // Squared distance between the position which they started the countdown, and their current position is larger than the allowed distance set in the Config.
            windupData.cancelled.set(true)

            windupData.players[0].sendSystemMessage(
                Component.literal(
                    MessageParser.enhancedFormatter(
                        Messages.PLAYER_MOVED_DURING_WINDUP.get(),
                        mapOf("command_used" to TPAPlusPlus.getCommandNameFromType(windupData.type))
                    )
                )
            )

            trackedWindupData.remove(windupData)
        }
    }
}

private fun getWindupDistance(windupData: WindupData): Double {
    return when (windupData.type) {
        CommandType.BACK -> {
            Config.BACK_WINDUP_DISTANCE.get()
        }

        CommandType.ACCEPT -> {
            Config.ACCEPT_WINDUP_DISTANCE.get()
        }

        CommandType.DENY -> {
            Config.DENY_WINDUP_DISTANCE.get()
        }

        CommandType.CANCEL -> {
            Config.CANCEL_WINDUP_DISTANCE.get()
        }

        CommandType.TPA -> {
            Config.TPA_WINDUP_DISTANCE.get()
        }

        CommandType.TPAHERE -> {
            Config.TPAHERE_WINDUP_DISTANCE.get()
        }

        CommandType.BLOCK -> {
            Config.BLOCK_WINDUP_DISTANCE.get()
        }

        CommandType.TOGGLE -> {
            Config.TOGGLE_WINDUP_DISTANCE.get()
        }

        CommandType.UNBLOCK -> {
            Config.UNBLOCK_WINDUP_DISTANCE.get()
        }
    }
}