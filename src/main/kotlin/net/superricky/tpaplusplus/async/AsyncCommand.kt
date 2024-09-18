package net.superricky.tpaplusplus.async

import net.superricky.tpaplusplus.utility.LevelBoundVec3
import net.superricky.tpaplusplus.utility.getDimension

interface AsyncCommand {
    fun checkWindupDistance(asyncCommandData: AsyncCommandData): Boolean

    fun getCooldownTime(): Double

    fun getDelayTime(): Double

    fun checkWindupDistance(
        asyncCommandData: AsyncCommandData,
        checkFunction: Function1<AsyncCommandData, Double>,
        minDistance: Double
    ): Boolean {
        if (minDistance < 0) {
            return true
        }
        val distance = checkFunction.invoke(asyncCommandData)
        if (distance == -1.0) {
            return false
        }
        return distance <= minDistance
    }

    fun getSenderDistance(asyncCommandData: AsyncCommandData): Double {
        val originPos = asyncCommandData.getPos()
        val sender = asyncCommandData.getRequest().sender
        val nowPos = LevelBoundVec3(sender.getDimension(), sender.pos)
        return originPos.distance(nowPos)
    }

    fun getReceiverDistance(asyncCommandData: AsyncCommandData): Double {
        val originPos = asyncCommandData.getPos()
        val receiver = asyncCommandData.getRequest().receiver
        require(receiver != null) { "Receiver not found" }
        val nowPos = LevelBoundVec3(receiver.getDimension(), receiver.pos)
        return originPos.distance(nowPos)
    }
}
