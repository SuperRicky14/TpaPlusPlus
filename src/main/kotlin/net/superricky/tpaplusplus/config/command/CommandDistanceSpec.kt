package net.superricky.tpaplusplus.config.command

import com.uchuhimo.konf.ConfigSpec

object CommandDistanceSpec : ConfigSpec("command.distance") {
    val denyDistance by required<Double>()
    val cancelDistance by required<Double>()
    val acceptDistance by required<Double>()
    val tpahereDistance by required<Double>()
    val backDistance by required<Double>()
    val blockDistance by required<Double>()
    val toggleDistance by required<Double>()
    val unblockDistance by required<Double>()
    val tpaDistance by required<Double>()
}
