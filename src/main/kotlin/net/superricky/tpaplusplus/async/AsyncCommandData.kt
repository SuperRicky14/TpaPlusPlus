package net.superricky.tpaplusplus.async

import kotlinx.atomicfu.AtomicBoolean
import kotlinx.atomicfu.atomic
import net.minecraft.server.network.ServerPlayerEntity
import net.superricky.tpaplusplus.config.config.CommonSpec
import net.superricky.tpaplusplus.config.config.Config.get
import net.superricky.tpaplusplus.config.language.LanguageConfig.getMutableText
import net.superricky.tpaplusplus.config.language.TeleportSpec
import net.superricky.tpaplusplus.config.language.WindupSpec
import net.superricky.tpaplusplus.utility.*

class AsyncCommandData(
    private val asyncRequest: AsyncRequest,
    private var pos: LevelBoundVec3,
    private val asyncCommandEventFactory: AsyncCommandEventFactory
) {
    private var canceled: AtomicBoolean = atomic(false)
    private var timeout = CommonSpec.tpaTimeout.get().translateSecondToTick()
    private var checkTarget = asyncRequest.sender

    init {
        asyncCommandEventFactory
            .addListener(AsyncCommandEvent.REQUEST_AFTER_DELAY) {
                AsyncCommandHelper.addCooldown(
                    asyncRequest.sender.uuid,
                    asyncRequest.commandType
                )
            }
            .addListener(AsyncCommandEvent.REQUEST_OUT_DISTANCE) {
                asyncRequest.sender.sendMessage(
                    WindupSpec.outDistance.getMutableText(
                        asyncRequest.commandType.handler.getCommandName()
                    ).setStyle(TextColorPallet.error)
                )
            }
            .addListener(AsyncCommandEvent.REQUEST_UPDATE_MESSAGE) {
                asyncRequest.sender.sendRemainTime(asyncRequest.delay)
            }
            .addListener(AsyncCommandEvent.REQUEST_UNDER_COOLDOWN) {
                asyncRequest.sender.sendCooldownTime(
                    asyncRequest.commandType.handler.getCommandName(),
                    asyncRequest.cooldown.translateTickToSecond()
                )
            }
        if (asyncRequest.isTeleportRequest()) {
            asyncCommandEventFactory
                .addListener(AsyncCommandEvent.TELEPORT_OUT_DISTANCE) {
                    asyncRequest.from?.sendMessage(
                        TeleportSpec.outDistance.getMutableText().setStyle(TextColorPallet.error)
                    )
                }
                .addListener(AsyncCommandEvent.TELEPORT_UPDATE_MESSAGE) {
                    asyncRequest.from?.sendTeleportTime(it.getRequest().delay)
                }
        }
    }

    fun needDelay(): Boolean = asyncRequest.delay != 0.0

    fun getDelay(): Double = asyncRequest.delay

    fun updateCurrentPos() {
        val from = asyncRequest.from
        from ?: return
        pos = LevelBoundVec3(from.getDimension(), from.pos)
    }

    fun setCheckTarget(checkTarget: ServerPlayerEntity) {
        this.checkTarget = checkTarget
    }

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
        asyncCommandEventFactory.invoke(commandResult, this)
    }

    fun cancel() {
        canceled.value = true
    }

    fun checkWindupDistance(): Boolean {
        val minDistance = asyncRequest.commandType.handler.getMinDistance()
        if (minDistance < 0) {
            return true
        }
        return pos.distance(LevelBoundVec3(checkTarget.getDimension(), checkTarget.pos)) <= minDistance
    }
}
