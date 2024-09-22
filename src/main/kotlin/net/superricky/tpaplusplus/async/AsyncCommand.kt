package net.superricky.tpaplusplus.async

import net.superricky.tpaplusplus.utility.LevelBoundVec3
import net.superricky.tpaplusplus.utility.getDimension

/**
 * Abstract class for asyncCommand
 */
abstract class AsyncCommand {
    @JvmField
    protected var commandName: String = ""

    /**
     * This function is used to check if the player's movement
     * distance is within the limit when the command is executed
     */
    abstract fun checkWindupDistance(asyncCommandData: AsyncCommandData): Boolean

    abstract fun getCooldownTime(): Double

    abstract fun getDelayTime(): Double

    /**
     * Command names cannot be hot loaded
     */
    fun getCommandName(): String = commandName

    protected fun checkWindupDistance(
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

    protected fun getSenderDistance(asyncCommandData: AsyncCommandData): Double {
        val originPos = asyncCommandData.getPos()
        val sender = asyncCommandData.getRequest().sender
        val nowPos = LevelBoundVec3(sender.getDimension(), sender.pos)
        return originPos.distance(nowPos)
    }

    protected fun getReceiverDistance(asyncCommandData: AsyncCommandData): Double {
        val originPos = asyncCommandData.getPos()
        val receiver = asyncCommandData.getRequest().receiver
        require(receiver != null) { "Receiver not found" }
        val nowPos = LevelBoundVec3(receiver.getDimension(), receiver.pos)
        return originPos.distance(nowPos)
    }
}
