package net.superricky.tpaplusplus.async

import kotlinx.atomicfu.AtomicBoolean
import kotlinx.atomicfu.atomic
import net.superricky.tpaplusplus.config.CommonSpec
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.async.request.Request
import net.superricky.tpaplusplus.utility.LevelBoundVec3
import net.superricky.tpaplusplus.utility.translateSecondToTick

class AsyncCommandData(
    private val request: Request,
    private val pos: LevelBoundVec3,
    private val callback: Function2<AsyncCommandResult, Request, Unit>
) {
    private var canceled: AtomicBoolean = atomic(false)
    private var timeout = Config.getConfig()[CommonSpec.tpaTimeout].toDouble().translateSecondToTick()

    fun needDelay(): Boolean = request.delay != 0.0

    fun getDelay(): Double = request.delay

    fun updateDelay(delay: Double) {
        request.delay = delay
    }

    fun updateCooldown(cooldown: Double) {
        request.cooldown = cooldown
    }

    fun tick(): Boolean {
        timeout--
        return timeout <= 0
    }

    fun getPos(): LevelBoundVec3 = pos

    fun getRequest(): Request = request

    fun isCanceled(): Boolean = canceled.value

    fun call(commandResult: AsyncCommandResult) {
        if (isCanceled()) {
            return
        }
        callback.invoke(commandResult, request)
    }

    fun cancel() {
        canceled.value = true
    }
}
