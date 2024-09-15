package net.superricky.tpaplusplus.windupcooldown.windup.impl

import net.minecraft.server.level.ServerPlayer
import net.superricky.tpaplusplus.commands.unblock.UnBlockPlayer
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.io.PlayerData
import net.superricky.tpaplusplus.windupcooldown.windup.AbstractWindup
import java.util.concurrent.atomic.AtomicInteger

class UnBlockPlayerWindup(
    private val executorData: PlayerData,
    val executor: ServerPlayer,
    private val blockedPlayer: ServerPlayer
) : AbstractWindup() {

    override val windupPlayer: ServerPlayer = executor
    override val onWindupFinished: () -> Unit = { UnBlockPlayer.absoluteUnBlockPlayer(executorData, executor, blockedPlayer) }
    override val acceptX: Double = executor.x
    override val acceptY: Double = executor.y
    override val acceptZ: Double = executor.z
    override val windupDelay: AtomicInteger = AtomicInteger(Config.UNBLOCK_WINDUP.get().toInt())
    override val windupDistance: Double = Config.UNBLOCK_WINDUP_DISTANCE.get()
    override val commandName: String = Config.TPUNBLOCK_COMMAND_NAME.get()
}