package net.superricky.tpaplusplus.config.command

import com.uchuhimo.konf.ConfigSpec

object CommandDelaySpec : ConfigSpec("command.delay") {
    val denyDelay by required<Double>()
    val cancelDelay by required<Double>()
    val acceptDelay by required<Double>()
    val tpahereDelay by required<Double>()
    val backDelay by required<Double>()
    val blockDelay by required<Double>()
    val toggleDelay by required<Double>()
    val unblockDelay by required<Double>()
    val tpaDelay by required<Double>()
}
