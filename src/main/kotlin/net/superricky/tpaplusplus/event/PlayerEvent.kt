package net.superricky.tpaplusplus.event

import dev.architectury.event.EventResult
import kotlinx.coroutines.launch
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.damage.DamageSource
import net.minecraft.server.network.ServerPlayerEntity
import net.superricky.tpaplusplus.TpaPlusPlus
import net.superricky.tpaplusplus.utility.LevelBoundVec3
import net.superricky.tpaplusplus.utility.getDimension

object PlayerEvent {
    fun joinEvent(player: ServerPlayerEntity) {
        TpaPlusPlus.dataService.insertPlayer(player.uuid)
    }

    fun deathEvent(deathEntity: LivingEntity, ignored: DamageSource): EventResult {
        if (deathEntity !is ServerPlayerEntity) {
            return EventResult.pass()
        }
        val deathPos = LevelBoundVec3(deathEntity.getDimension(), deathEntity.pos, deathEntity.yaw, deathEntity.pitch)
        TpaPlusPlus.launch {
            TpaPlusPlus.dataService.insertDeath(deathEntity.uuid, deathPos)
        }
        return EventResult.pass()
    }
}
