package net.superricky.tpaplusplus.config.language.command

import com.uchuhimo.konf.ConfigSpec

object BackSpec : ConfigSpec("back") {
    val deathNotFound by required<String>()
    val teleporting by required<String>()
    val teleported by required<String>()
}
