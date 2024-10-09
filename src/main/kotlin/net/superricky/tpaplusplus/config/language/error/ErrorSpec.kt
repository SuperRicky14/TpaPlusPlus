package net.superricky.tpaplusplus.config.language.error

import com.uchuhimo.konf.ConfigSpec

object ErrorSpec : ConfigSpec("error") {
    val senderNotExist by required<String>()
    val receiverNotExist by required<String>()
    val selfCommand by required<String>()
    val crossDim by required<String>()
    val tooFar by required<String>()
    val tooClose by required<String>()
}
