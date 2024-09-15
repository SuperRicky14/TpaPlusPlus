package net.superricky.tpaplusplus.windupcooldown.windup.impl

import net.minecraft.server.level.ServerPlayer
import net.superricky.tpaplusplus.commands.back.Back
import net.superricky.tpaplusplus.commands.back.LevelBoundVec3
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.windupcooldown.windup.AbstractWindup
import java.util.concurrent.atomic.AtomicInteger

class BackWindup(
    executor: ServerPlayer,
    deathPosition: LevelBoundVec3
) : AbstractWindup() {

    override val windupPlayer: ServerPlayer = executor
    override val onWindupFinished: () -> Unit = { Back.absoluteTeleportToLatestDeath(executor, deathPosition) }
    override val acceptX: Double = executor.x
    override val acceptY: Double = executor.y
    override val acceptZ: Double = executor.z
    override val windupDelay: AtomicInteger = AtomicInteger(Config.BACK_WINDUP.get().toInt())
    override val windupDistance: Double = Config.BACK_WINDUP_DISTANCE.get()
    override val commandName: String = Config.BACK_COMMAND_NAME.get()
}