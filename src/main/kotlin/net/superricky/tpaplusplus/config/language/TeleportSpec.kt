package net.superricky.tpaplusplus.config.language

import com.uchuhimo.konf.ConfigSpec

object TeleportSpec : ConfigSpec("teleport") {
    val remain by required<String>()
    val outDistance by required<String>()
}
