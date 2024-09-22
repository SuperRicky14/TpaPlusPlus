package net.superricky.tpaplusplus.config.command

import com.uchuhimo.konf.ConfigSpec

object CommandEnableSpec : ConfigSpec("command.enable") {
    val backEnable by required<Boolean>()
    val tpacancelEnable by required<Boolean>()
    val tpaunblockEnable by required<Boolean>()
    val tpablockEnable by required<Boolean>()
    val tpatoggleEnable by required<Boolean>()
    val tpahereEnable by required<Boolean>()
    val tpacceptEnable by required<Boolean>()
    val tpadenyEnable by required<Boolean>()
    val tpaEnable by required<Boolean>()
}
