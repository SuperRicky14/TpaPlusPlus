package net.superricky.tpaplusplus.config.command

import com.uchuhimo.konf.ConfigSpec

object CommandCooldownSpec : ConfigSpec("command.cooldown") {
    val globalCooldown by required<Double>()
    val denyCooldown by required<Double>()
    val cancelCooldown by required<Double>()
    val acceptCooldown by required<Double>()
    val tpahereCooldown by required<Double>()
    val backCooldown by required<Double>()
    val blockCooldown by required<Double>()
    val toggleCooldown by required<Double>()
    val unblockCooldown by required<Double>()
    val tpaCooldown by required<Double>()
}
