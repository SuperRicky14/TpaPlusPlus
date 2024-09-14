package net.superricky.tpaplusplus.async

import kotlinx.atomicfu.AtomicBoolean
import kotlinx.atomicfu.atomic
import net.superricky.tpaplusplus.config.AdvancedSpec
import net.superricky.tpaplusplus.config.CommonSpec
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.request.Request
import net.superricky.tpaplusplus.utility.LevelBoundVec3
import net.superricky.tpaplusplus.utility.AsyncCommandResult

class AsyncCommandData(
    private val request: Request,
    private val pos: LevelBoundVec3,
    val callback: Function1<AsyncCommandResult, Void>
) {
    private var canceled: AtomicBoolean = atomic(false)
    private var timeout = 0L

    init {
        timeout = Config.getConfig()[CommonSpec.tpaTimeout].toLong()
        if (Config.getConfig()[AdvancedSpec.unblockingTickLoop]) {
            timeout *= Config.getConfig()[AdvancedSpec.asyncLoopRate]
        } else {
            @Suppress("MagicNumber")
            timeout *= 20
        }
    }

    fun tick(): Boolean {
        timeout--
        return timeout <= 0
    }

    fun getPos(): LevelBoundVec3 = pos

    fun getRequest(): Request = request

    fun isCanceled(): Boolean = canceled.value

    fun cancel() {
        canceled.value = true
    }
}
