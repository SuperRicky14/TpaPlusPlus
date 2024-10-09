package net.superricky.tpaplusplus.config.config.command

import com.uchuhimo.konf.ConfigSpec

object CommandLimitationsSpec : ConfigSpec("command.limitations") {
    val crossDimAllowed by required<Boolean>()
    val maxTpDistance by required<Double>()
    val minTpDistance by required<Double>()
    val ignoreDistanceCrossDim by required<Boolean>()
}
