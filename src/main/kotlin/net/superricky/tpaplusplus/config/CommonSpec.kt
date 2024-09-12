package net.superricky.tpaplusplus.config

import com.uchuhimo.konf.ConfigSpec

object CommonSpec : ConfigSpec("common") {
    val showBlockedMessage by required<Boolean>()
    val toggledPlayerCommand by required<Boolean>()
    val tpaTimeout by required<Int>()
    val waitTimeBeforeTp by required<Int>()
}
