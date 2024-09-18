package net.superricky.tpaplusplus.async

import kotlinx.atomicfu.AtomicBoolean
import kotlinx.atomicfu.atomic
import kotlinx.coroutines.*
import net.superricky.tpaplusplus.command.commands.*
import net.superricky.tpaplusplus.utility.AsyncCommandResult
import net.superricky.tpaplusplus.utility.CommandType
import java.util.*
import kotlin.coroutines.CoroutineContext

object AsyncCommandHelper : CoroutineScope {
    override val coroutineContext: CoroutineContext = Dispatchers.IO
    private val ticking: AtomicBoolean = atomic(false)
    private val requests: MutableSet<AsyncCommandData> = Collections.synchronizedSet(HashSet())
    private val checkActions: Map<CommandType, Function1<AsyncCommandData, Boolean>> = mapOf(
        CommandType.TPA to TpaCommand::checkWindupDistance,
        CommandType.BACK to BackCommand::checkWindupDistance,
        CommandType.DENY to DenyCommand::checkWindupDistance,
        CommandType.BLOCK to BlockCommand::checkWindupDistance,
        CommandType.ACCEPT to AcceptCommand::checkWindupDistance,
        CommandType.CANCEL to CancelCommand::checkWindupDistance,
        CommandType.TOGGLE to ToggleCommand::checkWindupDistance,
        CommandType.TPAHERE to TpaHereCommand::checkWindupDistance,
        CommandType.UNBLOCK to UnblockCommand::checkWindupDistance
    )

    @Suppress("MagicNumber")
    fun startTickLoop(tickRate: Long) {
        ticking.value = true
        launch {
            while (isActive && ticking.value) {
                runTick()
                delay(1000L / tickRate)
            }
        }
    }

    fun stopTickLoop() {
        ticking.value = false
        coroutineContext.cancel()
    }

    fun schedule(request: AsyncCommandData) {
        requests.add(request)
    }

    fun runTick() {
        val elementRemoved = HashSet<AsyncCommandData>()
        requests.forEach {
            // check canceled
            if (it.isCanceled()) {
                it.call(AsyncCommandResult.BE_CANCELED)
                elementRemoved.add(it)
                return@forEach
            }
            // check distance
            if (!checkActions[it.getRequest().commandType]!!.invoke(it)) {
                it.call(AsyncCommandResult.OUT_OF_DISTANCE)
                elementRemoved.add(it)
                return@forEach
            }
            // check timeout
            if (it.tick()) {
                it.call(AsyncCommandResult.TIMEOUT)
                elementRemoved.add(it)
                return@forEach
            }
        }
        requests.removeAll(elementRemoved)
    }
}
