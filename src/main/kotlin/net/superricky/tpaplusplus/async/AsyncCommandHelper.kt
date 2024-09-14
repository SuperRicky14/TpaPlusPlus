package net.superricky.tpaplusplus.async

import kotlinx.atomicfu.AtomicBoolean
import kotlinx.atomicfu.atomic
import kotlinx.coroutines.*
import net.superricky.tpaplusplus.command.commands.TpaCommand
import net.superricky.tpaplusplus.utility.AsyncCommandResult
import net.superricky.tpaplusplus.utility.CommandType
import java.util.*
import kotlin.coroutines.CoroutineContext

object AsyncCommandHelper : CoroutineScope {
    override val coroutineContext: CoroutineContext = Dispatchers.IO
    private val ticking: AtomicBoolean = atomic(false)
    private val requests: MutableSet<AsyncCommandData> = Collections.synchronizedSet(HashSet())
    private val checkActions: Map<CommandType, Function1<AsyncCommandData, Boolean>> = mapOf(
        CommandType.TPA to TpaCommand::checkWindupDistance
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
                it.callback.invoke(AsyncCommandResult.BE_CANCELED)
                elementRemoved.add(it)
                return@forEach
            }
            // check distance
            if (!checkActions[it.getRequest().commandType]?.invoke(it)!!) {
                it.callback.invoke(AsyncCommandResult.OUT_OF_DISTANCE)
                elementRemoved.add(it)
                return@forEach
            }
            if (it.tick()) {
                it.callback.invoke(AsyncCommandResult.TIMEOUT)
                elementRemoved.add(it)
                return@forEach
            }
        }
        requests.removeAll(elementRemoved)
    }
}
