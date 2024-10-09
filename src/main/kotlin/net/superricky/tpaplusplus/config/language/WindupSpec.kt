package net.superricky.tpaplusplus.config.language

import com.uchuhimo.konf.ConfigSpec

object WindupSpec : ConfigSpec("windup") {
    val remain by required<String>()
    val outDistance by required<String>()
}
