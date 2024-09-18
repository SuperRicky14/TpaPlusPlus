package net.superricky.tpaplusplus.async

import kotlinx.atomicfu.AtomicBoolean
import kotlinx.atomicfu.atomic
import kotlinx.coroutines.*
import net.minecraft.server.network.ServerPlayerEntity
import net.superricky.tpaplusplus.GlobalConst.ONE_SECOND
import java.util.*
import kotlin.coroutines.CoroutineContext

object AsyncCommandHelper : CoroutineScope {
    override val coroutineContext: CoroutineContext = Dispatchers.IO
    private val ticking: AtomicBoolean = atomic(false)
    private val requests: MutableSet<AsyncCommandData> = Collections.synchronizedSet(HashSet())
    private val underCooldown: MutableMap<UUID, MutableMap<AsyncCommandType, Double>> =
        Collections.synchronizedMap(HashMap())
    private lateinit var mainLoopJob: Job
    private var tickDelay = 0L
    private var tickRate = 0L

    fun startTickLoop(tickRate: Long) {
        ticking.value = true
        this.tickRate = tickRate
        tickDelay = ONE_SECOND / tickRate
        mainLoopJob = launch {
            while (isActive && ticking.value) {
                runTick()
                delay(tickDelay)
            }
        }
    }

    fun stopTickLoop() {
        ticking.value = false
        mainLoopJob.cancel()
        coroutineContext.cancel()
    }

    fun addCooldown(uuid: UUID, type: AsyncCommandType) {
        val playerData = underCooldown[uuid]
        if (playerData == null) {
            underCooldown[uuid] = mutableMapOf(
                type to type.handler.getCooldownTime() * tickRate
            )
            return
        }
        playerData[type] = type.handler.getCooldownTime() * tickRate
    }

    fun schedule(request: AsyncCommandData) {
        val uuid = request.getRequest().sender.uuid
        val playerData = underCooldown[uuid]
        if (playerData == null) {
            underCooldown[uuid] = mutableMapOf()
        } else if (playerData[request.getRequest().commandType] != null) {
            request.updateCooldown(playerData[request.getRequest().commandType]!!)
            request.call(AsyncCommandResult.UNDER_COOLDOWN)
            return
        }
        if (request.needDelay()) {
            var delayTime = request.getDelay()
            val job = launch {
                while (true) {
                    delay(tickDelay)
                    if (!request.getRequest().commandType.handler.checkWindupDistance(request)) {
                        request.call(AsyncCommandResult.OUT_OF_DISTANCE)
                        request.cancel()
                        return@launch
                    }
                }
            }
            launch {
                request.call(AsyncCommandResult.UPDATE_DELAY_MESSAGE)
                while (true) {
                    delay(ONE_SECOND)
                    if (request.isCanceled()) {
                        return@launch
                    }
                    delayTime -= 1
                    request.updateDelay(delayTime)
                    request.call(AsyncCommandResult.UPDATE_DELAY_MESSAGE)
                    if (delayTime < 1.0) {
                        break
                    }
                }
                delay((delayTime * ONE_SECOND).toLong())
                request.call(AsyncCommandResult.AFTER_DELAY)
                job.cancel()
                requests.add(request)
            }
        } else {
            request.call(AsyncCommandResult.AFTER_DELAY)
            requests.add(request)
        }
    }

    fun acceptRequest(receiver: ServerPlayerEntity): AsyncCommandResult {
        val request = requests.find { it.getRequest().receiver == receiver }
        if (request == null) {
            return AsyncCommandResult.REQUEST_NOT_FOUND
        }
        request.cancel()
        request.call(AsyncCommandResult.REQUEST_ACCEPTED)
        return AsyncCommandResult.ACCEPT_SUCCESS
    }

    fun acceptRequest(receiver: ServerPlayerEntity, sender: ServerPlayerEntity): AsyncCommandResult {
        val request = requests.find { it.getRequest().receiver == receiver && it.getRequest().sender == sender }
        if (request == null) {
            return AsyncCommandResult.REQUEST_NOT_FOUND
        }
        request.cancel()
        request.call(AsyncCommandResult.REQUEST_ACCEPTED)
        return AsyncCommandResult.ACCEPT_SUCCESS
    }

    fun runTick() {
        val elementRemoved = HashSet<AsyncCommandData>()
        requests.forEach {
            // check canceled
            if (it.isCanceled()) {
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
        // update cooldown
        underCooldown.forEach { (key, value) ->
            for (entry in value) {
                entry.setValue(entry.value - 1)
            }
            underCooldown[key] = value.filter { it.value > 0 }.toMutableMap()
        }
    }
}
