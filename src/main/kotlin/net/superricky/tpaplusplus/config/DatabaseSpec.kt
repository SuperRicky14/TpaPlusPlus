package net.superricky.tpaplusplus.config

import com.uchuhimo.konf.ConfigSpec

object DatabaseSpec : ConfigSpec("database") {
    val location by optional<String?>(null)
}
