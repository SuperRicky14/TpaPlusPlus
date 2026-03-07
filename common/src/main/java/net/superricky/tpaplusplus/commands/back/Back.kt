package net.superricky.tpaplusplus.commands.back

import net.minecraft.network.chat.Component
import net.minecraft.server.level.ServerPlayer
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.Messages
import net.superricky.tpaplusplus.cooldown.CommandType
import net.superricky.tpaplusplus.cooldown.CooldownManager
import net.superricky.tpaplusplus.cooldown.CooldownManager.getPlayerCooldown
import net.superricky.tpaplusplus.cooldown.CooldownManager.scheduleCooldown
import java.time.Duration

object Back {
    fun teleportToLatestDeath(executor: ServerPlayer) {
        if (!Config.BACK_COMMAND_ENABLED.get()) {
            executor.sendSystemMessage(Component.literal(Messages.ERR_BACK_COMMAND_DISABLED.get()))
            return
        }

        val deathPosition = DeathHelper.playerDeathCoordinates[executor.getUUID()]

        if (deathPosition == null) {
            executor.sendSystemMessage(Component.literal(Messages.ERR_DEATH_LOC_NOT_FOUND.get()))
            return
        }

        getPlayerCooldown(executor.getUUID(), CommandType.BACK)?.let { cooldown ->
            CooldownManager.notifyCooldown(executor, cooldown)
            return
        }

        if (Config.BACK_COOLDOWN.get() > 0)  // Check if cooldown is enabled
            scheduleCooldown(
                executor.getUUID(),
                Duration.ofSeconds(Config.BACK_COOLDOWN.get().toLong()),
                CommandType.BACK
            )

        absoluteTeleportToLatestDeath(executor, deathPosition)
    }

    fun absoluteTeleportToLatestDeath(executor: ServerPlayer, deathPosition: LevelBoundVec3) {
        executor.sendSystemMessage(Component.literal(Messages.DEATH_BEING_TELEPORTED.get()))

        teleportToLastPosition(executor, deathPosition)
        DeathHelper.removePlayerDeathCoordinates(executor.getUUID())

        executor.sendSystemMessage(Component.literal(Messages.DEATH_TELEPORTED.get()))
    }

    fun teleportToLastPosition(executor: ServerPlayer, deathPosition: LevelBoundVec3) {
        executor.teleportTo(
            deathPosition.serverLevel,
            deathPosition.x,
            deathPosition.y,
            deathPosition.z,
            executor.yRot,
            executor.xRot
        )
    }
}
