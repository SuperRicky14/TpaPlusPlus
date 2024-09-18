package net.superricky.tpaplusplus.config

import com.uchuhimo.konf.ConfigSpec

object ColorSpec : ConfigSpec("common.color") {
    val primary by required<String>()
    val primaryVariant by required<String>()
    val secondary by required<String>()
    val secondaryVariant by required<String>()
    val error by required<String>()
}
