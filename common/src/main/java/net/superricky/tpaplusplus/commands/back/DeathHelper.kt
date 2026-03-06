package net.superricky.tpaplusplus.commands.back

import dev.architectury.event.EventResult
import net.minecraft.server.level.ServerPlayer
import net.minecraft.world.entity.LivingEntity
import java.util.*

object DeathHelper {
    val playerDeathCoordinates: MutableMap<UUID, LevelBoundVec3> = mutableMapOf()

    fun removePlayerDeathCoordinates(executorUUID: UUID) {
        playerDeathCoordinates.remove(executorUUID)
    }

    /**
     * Listen for death events and log player deaths in playerDeathCoordinates.
     * Necessary so /back knows where the player previously died!
     */
    fun onDeath(deadEntity: LivingEntity): EventResult {
        if (deadEntity !is ServerPlayer) {
            return EventResult.pass() // Don't care about non-player entities
        }

        val deathPosition = LevelBoundVec3(deadEntity.serverLevel(), deadEntity.x, deadEntity.y, deadEntity.z)

        playerDeathCoordinates[deadEntity.getUUID()] = deathPosition

        return EventResult.pass()
    }

    fun clearDeathCoordinates() {
        playerDeathCoordinates.clear()
    }
}
