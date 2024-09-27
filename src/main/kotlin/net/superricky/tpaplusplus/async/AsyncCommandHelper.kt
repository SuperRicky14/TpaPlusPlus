package net.superricky.tpaplusplus.async

import kotlinx.atomicfu.AtomicBoolean
import kotlinx.atomicfu.atomic
import kotlinx.coroutines.*
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.text.Text
import net.superricky.tpaplusplus.GlobalConst.ONE_SECOND
import net.superricky.tpaplusplus.GlobalConst.logger
import net.superricky.tpaplusplus.TpaPlusPlus
import net.superricky.tpaplusplus.config.CommonSpec
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.utility.TextColorPallet
import net.superricky.tpaplusplus.utility.getWorld
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
                launch {
                    try {
                        runTick()
                    } catch (e: Exception) {
                        logger.error(e)
                    }
                }
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
        // Check if cooldown is disabled
        if (type.handler.getCooldownTime() == 0.0) {
            return
        }
        val playerData = underCooldown[uuid]
        playerData?.let {
            it[type] = type.handler.getCooldownTime() * tickRate
            return
        }
        underCooldown[uuid] = mutableMapOf(type to type.handler.getCooldownTime() * tickRate)
    }

    private fun asyncWindupCheck(
        asyncCommandData: AsyncCommandData,
        successCallback: Function1<AsyncCommandData, Unit>? = null,
        errorCallback: Function1<AsyncCommandData, Unit>? = null,
        progressCallback: Function1<AsyncCommandData, Unit>? = null,
        delay: Double? = null
    ) {
        val job = launch {
            while (true) {
                delay(tickDelay)
                if (!asyncCommandData.checkWindupDistance()) {
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
                if (delayTime < 1.0) break
                progressCallback?.invoke(asyncCommandData)
            }
            if (delayTime != 0.0) {
                progressCallback?.invoke(asyncCommandData)
                delay((delayTime * ONE_SECOND).toLong())
            }
            if (asyncCommandData.isCanceled()) {
                return@launch
            }
            job.cancel()
            successCallback?.invoke(asyncCommandData)
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
            // If request is not acceptable, which means it do not need someone to confirm execute
            // then there are no necessary to consider request timeout
            if (request.getRequest().acceptable()) {
                requests.add(request)
            }
            return
        }
        asyncWindupCheck(
            request,
            successCallback = {
                it.call(AsyncCommandEvent.REQUEST_AFTER_DELAY)
                if (it.getRequest().acceptable()) {
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

    fun checkRequestExist(
        sender: ServerPlayerEntity,
        receiver: ServerPlayerEntity,
        commandType: AsyncCommandType
    ): Boolean {
        return requests.find {
            it.getRequest().sender == sender &&
                    it.getRequest().receiver == receiver &&
                    it.getRequest().commandType == commandType
        } != null
    }

    private fun ServerPlayerEntity.backTeleport() {
        val lastDeathPos = TpaPlusPlus.dataService.getPlayerData(this@backTeleport).lastDeathPos
        this@backTeleport.teleport(
            TpaPlusPlus.server.getWorld(lastDeathPos.world.getWorld()),
            lastDeathPos.x,
            lastDeathPos.y,
            lastDeathPos.z,
            lastDeathPos.yaw,
            lastDeathPos.pitch
        )
        lastDeathPos.backed = true
        this@backTeleport.sendMessage(
            Text.translatable("command.back.teleported").setStyle(TextColorPallet.primary)
        )
    }

    private fun ServerPlayerEntity.teleport(target: ServerPlayerEntity) {
        this.teleport(target.serverWorld, target.x, target.y, target.z, target.yaw, target.pitch)
    }

    fun teleport(asyncCommandData: AsyncCommandData) {
        launch {
            val asyncRequest = asyncCommandData.getRequest()
            if (Config.getConfig()[CommonSpec.waitTimeBeforeTp] == 0.0) {
                if (asyncRequest.commandType == AsyncCommandType.BACK) {
                    asyncRequest.sender.backTeleport()
                } else {
                    asyncRequest.from!!.teleport(asyncRequest.to!!)
                }
                return@launch
            }
            asyncCommandData.updateCurrentPos()
            asyncCommandData.setCheckTarget(asyncCommandData.getRequest().from!!)
            asyncWindupCheck(
                asyncCommandData,
                successCallback = {
                    if (asyncRequest.commandType == AsyncCommandType.BACK) {
                        asyncRequest.sender.backTeleport()
                    } else {
                        asyncRequest.from!!.teleport(asyncRequest.to!!)
                    }
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

    private fun acceptRequest(request: AsyncCommandData?): AsyncCommandEvent {
        request ?: return AsyncCommandEvent.REQUEST_NOT_FOUND
        requests.remove(request)
        request.call(AsyncCommandEvent.REQUEST_ACCEPTED)
        return AsyncCommandEvent.USELESS_VOID
    }

    fun acceptRequest(receiver: ServerPlayerEntity): AsyncCommandEvent {
        return acceptRequest(requests.find { it.getRequest().receiver == receiver })
    }

    fun acceptRequest(receiver: ServerPlayerEntity, sender: ServerPlayerEntity): AsyncCommandEvent {
        return acceptRequest(requests.find { it.getRequest().receiver == receiver && it.getRequest().sender == sender })
    }

    private fun cancelRequest(request: AsyncCommandData?): AsyncCommandEvent {
        request ?: return AsyncCommandEvent.REQUEST_NOT_FOUND
        requests.remove(request)
        request.call(AsyncCommandEvent.REQUEST_CANCELED)
        return AsyncCommandEvent.USELESS_VOID
    }

    fun cancelRequest(sender: ServerPlayerEntity): AsyncCommandEvent {
        return cancelRequest(requests.find { it.getRequest().sender == sender })
    }

    fun cancelRequest(sender: ServerPlayerEntity, receiver: ServerPlayerEntity): AsyncCommandEvent {
        return cancelRequest(requests.find { it.getRequest().sender == sender && it.getRequest().receiver == receiver })
    }

    private fun refuseRequest(request: AsyncCommandData?): AsyncCommandEvent {
        request ?: return AsyncCommandEvent.REQUEST_NOT_FOUND
        requests.remove(request)
        request.call(AsyncCommandEvent.REQUEST_REFUSED)
        return AsyncCommandEvent.USELESS_VOID
    }

    fun refuseRequest(receiver: ServerPlayerEntity): AsyncCommandEvent {
        return refuseRequest(requests.find { it.getRequest().receiver == receiver })
    }

    fun refuseRequest(receiver: ServerPlayerEntity, sender: ServerPlayerEntity): AsyncCommandEvent {
        return refuseRequest(requests.find { it.getRequest().receiver == receiver && it.getRequest().sender == sender })
    }
}
