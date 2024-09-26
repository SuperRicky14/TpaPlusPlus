package net.superricky.tpaplusplus.async

/**
 * Abstract class for asyncCommand
 */
abstract class AsyncCommand {
    @JvmField
    protected var commandName: String = ""

    /**
     * Command names cannot be hot loaded
     */
    fun getCommandName(): String = commandName

    abstract fun getCooldownTime(): Double

    abstract fun getDelayTime(): Double

    abstract fun getMinDistance(): Double
}
