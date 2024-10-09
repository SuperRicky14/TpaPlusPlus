package net.superricky.tpaplusplus.config.language

import com.uchuhimo.konf.ConfigSpec

object CooldownSpec : ConfigSpec("cooldown") {
    val underCooldown by required<String>()
}
