package net.superricky.tpaplusplus.config.language.command

import com.uchuhimo.konf.ConfigSpec

object TpaHereSpec : ConfigSpec("tpaHere") {
    val requestSender by required<String>()
    val requestReceiver by required<String>()
    val timeoutSender by required<String>()
    val timeoutReceiver by required<String>()
    val acceptSender by required<String>()
    val acceptReceiver by required<String>()
    val cancelSender by required<String>()
    val cancelReceiver by required<String>()
    val refuseSender by required<String>()
    val refuseReceiver by required<String>()
}
