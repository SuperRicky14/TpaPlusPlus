package net.superricky.tpaplusplus.windupcooldown.windup.impl

import net.minecraft.server.level.ServerPlayer
import net.superricky.tpaplusplus.commands.toggle.TPToggle
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.windupcooldown.windup.AbstractWindup
import java.util.concurrent.atomic.AtomicInteger

class TPToggleWindup(
    val executor: ServerPlayer
) : AbstractWindup() {

    override val windupPlayer: ServerPlayer = executor
    override val onWindupFinished: () -> Unit = { TPToggle.toggleTP(executor) }
    override val acceptX: Double = executor.x
    override val acceptY: Double = executor.y
    override val acceptZ: Double = executor.z
    override val windupDelay: AtomicInteger = AtomicInteger(Config.TOGGLE_WINDUP.get().toInt())
    override val windupDistance: Double = Config.TOGGLE_WINDUP_DISTANCE.get()
    override val commandName: String = Config.TPTOGGLE_COMMAND_NAME.get()
}