package net.superricky.tpaplusplus.config.language.command

import com.uchuhimo.konf.ConfigSpec

object ToggleSpec : ConfigSpec("toggle") {
    val on by required<String>()
    val off by required<String>()
}
