package net.superricky.tpaplusplus.windupcooldown.windup

import kotlinx.coroutines.CoroutineDispatcher
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import net.minecraft.network.chat.Component
import net.superricky.tpaplusplus.TPAPlusPlus
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.Messages
import net.superricky.tpaplusplus.config.formatters.MessageParser
import net.superricky.tpaplusplus.windupcooldown.CommandType
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.util.Map
import java.util.concurrent.ConcurrentHashMap
import kotlin.math.pow

private val LOGGER: Logger = LoggerFactory.getLogger(TPAPlusPlus.MOD_ID)
private val dispatcher: CoroutineDispatcher = Dispatchers.Default
private val scope: CoroutineScope = CoroutineScope(dispatcher)
private val trackedWindupData: MutableSet<WindupData> = ConcurrentHashMap.newKeySet()

fun runTick() {
    while (true) {
        for (windupData in trackedWindupData) {
            val windupDistance = getWindupDistance(windupData)

            if (windupDistance == -1.0) continue

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
                            Map.of("command_used", TPAPlusPlus.getCommandNameFromType(windupData.type))
                        )
                    )
                )

                trackedWindupData.remove(windupData)
            }
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