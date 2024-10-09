package net.superricky.tpaplusplus.config.language.command

import com.uchuhimo.konf.ConfigSpec

object BlockSpec : ConfigSpec("block") {
    val success by required<String>()
    val failure by required<String>()
    val blockedPlayer by required<String>()
}
