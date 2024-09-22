package net.superricky.tpaplusplus.async

import kotlinx.atomicfu.AtomicBoolean
import kotlinx.atomicfu.atomic
import kotlinx.coroutines.*
import net.minecraft.server.network.ServerPlayerEntity
import net.superricky.tpaplusplus.GlobalConst.ONE_SECOND
import net.superricky.tpaplusplus.config.CommonSpec
import net.superricky.tpaplusplus.config.Config
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

    fun asyncWindupCheck(
        asyncCommandData: AsyncCommandData,
        successCallback: Function1<AsyncCommandData, Unit>? = null,
        errorCallback: Function1<AsyncCommandData, Unit>? = null,
        progressCallback: Function1<AsyncCommandData, Unit>? = null,
        delay: Double? = null
    ) {
        val job = launch {
            while (true) {
                delay(tickDelay)
                if (!asyncCommandData.getRequest().commandType.handler.checkWindupDistance(asyncCommandData)) {
                    errorCallback?.invoke(asyncCommandData)
                    return@launch
                }
            }
        }
        var delayTime = delay ?: asyncCommandData.getDelay()
        asyncCommandData.updateDelay(delayTime)
        launch {
            progressCallback?.invoke(asyncCommandData)
            while (true) {
                delay(ONE_SECOND)
                if (asyncCommandData.isCanceled()) {
                    return@launch
                }
                delayTime -= 1
                asyncCommandData.updateDelay(delayTime)
                progressCallback?.invoke(asyncCommandData)
                if (delayTime < 1.0) {
                    break
                }
            }
            delay((delayTime * ONE_SECOND).toLong())
            if (asyncCommandData.isCanceled()) {
                return@launch
            }
            job.cancel()
            successCallback?.invoke(asyncCommandData)
        }
    }

    private fun teleportPlayer(from: ServerPlayerEntity, to: ServerPlayerEntity) =
        from.teleport(to.serverWorld, to.x, to.y, to.z, to.yaw, to.pitch)

    fun teleport(asyncCommandData: AsyncCommandData) {
        launch {
            val asyncRequest = asyncCommandData.getRequest()
            if (Config.getConfig()[CommonSpec.waitTimeBeforeTp] == 0.0) {
                teleportPlayer(asyncRequest.from!!, asyncRequest.to!!)
                return@launch
            }
            asyncWindupCheck(
                asyncCommandData,
                successCallback = {
                    teleportPlayer(asyncRequest.from!!, asyncRequest.to!!)
                    it.cancel()
                },
                errorCallback = {
                    it.call(AsyncCommandEvent.TELEPORT_OUT_DISTANCE)
                    it.cancel()
                },
                progressCallback = {
                    it.call(AsyncCommandEvent.TELEPORT_UPDATE_MESSAGE)
                },
                Config.getConfig()[CommonSpec.waitTimeBeforeTp]
            )
        }
    }

    fun schedule(request: AsyncCommandData) {
        val uuid = request.getRequest().sender.uuid
        val playerData = underCooldown[uuid]
        if (playerData == null) {
            underCooldown[uuid] = mutableMapOf()
        } else if (playerData[request.getRequest().commandType] != null) {
            request.updateCooldown(playerData[request.getRequest().commandType]!!)
            request.call(AsyncCommandEvent.REQUEST_UNDER_COOLDOWN)
            return
        }
        if (!request.needDelay()) {
            request.call(AsyncCommandEvent.REQUEST_AFTER_DELAY)
            // If request not a teleport request, then there are no necessary to consider timeout
            if (request.getRequest().isTeleportRequest()) {
                requests.add(request)
            }
            return
        }
        asyncWindupCheck(
            request,
            successCallback = {
                it.call(AsyncCommandEvent.REQUEST_AFTER_DELAY)
                if (it.getRequest().isTeleportRequest()) {
                    requests.add(it)
                }
            },
            errorCallback = {
                it.call(AsyncCommandEvent.REQUEST_OUT_DISTANCE)
                it.cancel()
            },
            progressCallback = {
                it.call(AsyncCommandEvent.REQUEST_UPDATE_MESSAGE)
            }
        )
    }

    fun acceptRequest(receiver: ServerPlayerEntity): AsyncCommandEvent {
        val request = requests.find { it.getRequest().receiver == receiver }
        if (request == null) {
            return AsyncCommandEvent.REQUEST_NOT_FOUND
        }
        requests.remove(request)
        request.call(AsyncCommandEvent.REQUEST_ACCEPTED)
        return AsyncCommandEvent.USELESS_VOID
    }

    fun acceptRequest(receiver: ServerPlayerEntity, sender: ServerPlayerEntity): AsyncCommandEvent {
        val request = requests.find { it.getRequest().receiver == receiver && it.getRequest().sender == sender }
        if (request == null) {
            return AsyncCommandEvent.REQUEST_NOT_FOUND
        }
        requests.remove(request)
        request.call(AsyncCommandEvent.REQUEST_ACCEPTED)
        return AsyncCommandEvent.USELESS_VOID
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
                it.call(AsyncCommandEvent.REQUEST_TIMEOUT)
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
