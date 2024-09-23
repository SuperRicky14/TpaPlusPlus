package net.superricky.tpaplusplus.async

import kotlinx.atomicfu.AtomicBoolean
import kotlinx.atomicfu.atomic
import net.minecraft.text.Text
import net.superricky.tpaplusplus.config.CommonSpec
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.utility.*

class AsyncCommandData(
    private val asyncRequest: AsyncRequest,
    private var pos: LevelBoundVec3,
    private val callback: Function2<AsyncCommandEvent, AsyncCommandData, Unit>
) {
    private var canceled: AtomicBoolean = atomic(false)
    private var timeout = Config.getConfig()[CommonSpec.tpaTimeout].translateSecondToTick()

    fun needDelay(): Boolean = asyncRequest.delay != 0.0

    fun getDelay(): Double = asyncRequest.delay

    fun updateDelay(delay: Double) {
        asyncRequest.delay = delay
    }

    fun updateCooldown(cooldown: Double) {
        asyncRequest.cooldown = cooldown
    }

    fun tick(): Boolean {
        timeout--
        return timeout <= 0
    }

    fun getPos(): LevelBoundVec3 = pos

    fun getRequest(): AsyncRequest = asyncRequest

    fun isCanceled(): Boolean = canceled.value

    fun call(commandResult: AsyncCommandEvent) {
        if (isCanceled()) {
            return
        }
        when (commandResult) {
            AsyncCommandEvent.REQUEST_OUT_DISTANCE -> {
                asyncRequest.sender.sendMessage(
                    Text.translatable(
                        "command.windup.error.out_distance",
                        asyncRequest.commandType.handler.getCommandName()
                    ).setStyle(TextColorPallet.error)
                )
            }

            AsyncCommandEvent.REQUEST_UPDATE_MESSAGE -> {
                asyncRequest.sender.sendRemainTime(asyncRequest.delay)
            }

            AsyncCommandEvent.REQUEST_UNDER_COOLDOWN -> {
                asyncRequest.sender.sendCooldownTime(
                    asyncRequest.commandType.handler.getCommandName(),
                    asyncRequest.cooldown.translateTickToSecond()
                )
            }

            else -> {
                callback.invoke(commandResult, this)
            }
        }
    }

    fun cancel() {
        canceled.value = true
    }
}
