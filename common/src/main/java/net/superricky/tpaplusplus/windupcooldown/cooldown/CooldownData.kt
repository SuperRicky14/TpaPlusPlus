package net.superricky.tpaplusplus.windupcooldown.cooldown

import net.superricky.tpaplusplus.windupcooldown.CommandType
import java.util.UUID

data class CooldownData(
    val playerIds: Array<UUID>,
    val cooldownType: CommandType,
    val delay: Int
)