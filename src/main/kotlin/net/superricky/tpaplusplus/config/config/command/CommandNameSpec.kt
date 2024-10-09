package net.superricky.tpaplusplus.config.config.command

import com.uchuhimo.konf.ConfigSpec

object CommandNameSpec : ConfigSpec("command.name") {
    val backCommand by required<String>()
    val tpacancelCommand by required<String>()
    val tpaunblockCommand by required<String>()
    val tpablockCommand by required<String>()
    val tpatoggleCommand by required<String>()
    val tpahereCommand by required<String>()
    val tpacceptCommand by required<String>()
    val tpadenyCommand by required<String>()
    val tpaCommand by required<String>()
}
