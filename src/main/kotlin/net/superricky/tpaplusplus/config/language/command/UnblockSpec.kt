package net.superricky.tpaplusplus.config.language.command

import com.uchuhimo.konf.ConfigSpec

object UnblockSpec : ConfigSpec("unblock") {
    val success by required<String>()
    val failure by required<String>()
    val unblockedPlayer by required<String>()
}
