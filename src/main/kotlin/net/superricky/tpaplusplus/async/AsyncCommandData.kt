package net.superricky.tpaplusplus.async

import kotlinx.atomicfu.AtomicBoolean
import kotlinx.atomicfu.atomic
import net.superricky.tpaplusplus.config.CommonSpec
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.request.Request
import net.superricky.tpaplusplus.utility.AsyncCommandResult
import net.superricky.tpaplusplus.utility.LevelBoundVec3
import net.superricky.tpaplusplus.utility.translateSecondToTick
import net.superricky.tpaplusplus.utility.translateTickToSecond

class AsyncCommandData(
    private val request: Request,
    private val pos: LevelBoundVec3,
    private var delay: Double,
    private val callback: Function2<AsyncCommandResult, Request, Unit>
) {
    private var canceled: AtomicBoolean = atomic(false)
    private var timeout = 0L
    private var afterDelay: Boolean = false

    init {
        timeout = Config.getConfig()[CommonSpec.tpaTimeout].toLong().translateSecondToTick()
        delay = delay.translateTickToSecond()
    }

    fun tick(): Boolean {
        if (delay > 0) {
            delay--
            return false
        }
        if (!afterDelay) {
            afterDelay = true
            call(AsyncCommandResult.AFTER_DELAY)
        }
        timeout--
        return timeout <= 0
    }

    fun getPos(): LevelBoundVec3 = pos

    fun getRequest(): Request = request

    fun isCanceled(): Boolean = canceled.value

    fun call(commandResult: AsyncCommandResult) {
        callback.invoke(commandResult, request)
    }

    fun cancel() {
        canceled.value = true
    }
}
