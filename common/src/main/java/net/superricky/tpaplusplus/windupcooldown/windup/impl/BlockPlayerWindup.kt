package net.superricky.tpaplusplus.windupcooldown.windup.impl

import net.minecraft.server.level.ServerPlayer
import net.superricky.tpaplusplus.commands.block.BlockPlayer
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.io.PlayerData
import net.superricky.tpaplusplus.windupcooldown.windup.AbstractWindup
import java.util.concurrent.atomic.AtomicInteger

class BlockPlayerWindup(
    executorData: PlayerData,
    executor: ServerPlayer,
    blockedPlayer: ServerPlayer
) : AbstractWindup() {

    override val windupPlayer: ServerPlayer = executor
    override val onWindupFinished: () -> Unit = { BlockPlayer.absoluteBlockPlayer(executorData, executor, blockedPlayer) }
    override val acceptX: Double = executor.x
    override val acceptY: Double = executor.y
    override val acceptZ: Double = executor.z
    override val windupDelay: AtomicInteger = AtomicInteger(Config.BLOCK_WINDUP.get().toInt())
    override val windupDistance: Double = Config.BLOCK_WINDUP_DISTANCE.get()
    override val commandName: String = Config.TPBLOCK_COMMAND_NAME.get()
}