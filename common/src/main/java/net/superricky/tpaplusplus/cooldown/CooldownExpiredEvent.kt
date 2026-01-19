package net.superricky.tpaplusplus.cooldown

import dev.architectury.event.Event
import dev.architectury.event.EventFactory
import dev.architectury.event.EventResult
import net.minecraft.server.MinecraftServer

interface CooldownExpiredEvent {
    fun cooldownExpired(cooldownData: CooldownData, server: MinecraftServer): EventResult

    companion object {
        val EVENT: Event<CooldownExpiredEvent> = EventFactory.createEventResult()
    }
}