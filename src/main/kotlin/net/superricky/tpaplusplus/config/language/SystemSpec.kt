package net.superricky.tpaplusplus.config.language

import com.uchuhimo.konf.ConfigSpec

object SystemSpec : ConfigSpec("system") {
    val version by required<String>()
    val githubBase by required<String>()
    val githubView by required<String>()
    val modrinthBase by required<String>()
    val modrinthView by required<String>()
    val courseforgeBase by required<String>()
    val courseforgeView by required<String>()
    val reloadStart by required<String>()
    val reloadFinish by required<String>()
}
