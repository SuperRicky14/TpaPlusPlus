package net.superricky.tpaplusplus.config.language.error

import com.uchuhimo.konf.ConfigSpec

object ErrorRequestSpec : ConfigSpec("error.request") {
    val notfoundAll by required<String>()
    val notfoundTarget by required<String>()
    val blockedSender by required<String>()
    val blockedReceiver by required<String>()
    val toggledSender by required<String>()
    val toggledReceiver by required<String>()
    val requestExist by required<String>()
}
