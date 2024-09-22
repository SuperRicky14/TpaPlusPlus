package net.superricky.tpaplusplus.utility

import dev.architectury.event.EventResult
import kotlinx.coroutines.launch
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.damage.DamageSource
import net.minecraft.server.network.ServerPlayerEntity
import net.superricky.tpaplusplus.TpaPlusPlus

object PlayerEvent {
    fun joinEvent(player: ServerPlayerEntity) {
        TpaPlusPlus.dataService.insertPlayer(player.uuid)
    }

    fun deathEvent(deathEntity: LivingEntity, ignored: DamageSource): EventResult {
        if (deathEntity !is ServerPlayerEntity) {
            return EventResult.pass()
        }
        val deathPos = LevelBoundVec3(deathEntity.getDimension(), deathEntity.pos)
        TpaPlusPlus.launch {
            TpaPlusPlus.dataService.insertDeath(deathEntity.uuid, deathPos)
        }
        return EventResult.pass()
    }
}
