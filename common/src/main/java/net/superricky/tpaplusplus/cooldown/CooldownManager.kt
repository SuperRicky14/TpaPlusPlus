package net.superricky.tpaplusplus.cooldown

import dev.architectury.event.EventResult
import net.minecraft.network.chat.Component
import net.minecraft.server.MinecraftServer
import net.minecraft.server.level.ServerPlayer
import net.superricky.tpaplusplus.config.Messages
import net.superricky.tpaplusplus.util.MsgFmt
import java.time.Duration
import java.time.Instant
import java.util.*

data class CooldownData(val playerCooldownUUID: UUID, val cooldownTimestamp: Instant, val commandOnCooldown: CommandType)

object CooldownManager {
    private val cooldowns: MutableList<CooldownData> = mutableListOf()

    fun scheduleCooldown(playerUUID: UUID, delay: Duration, type: CommandType) {
        val cooldownData = CooldownData(playerUUID, Instant.now() + delay, type)

        cooldowns.add(cooldownData)
    }

    fun onMinecraftServerTick(server: MinecraftServer) {
        cooldowns.removeIf {cooldownData ->
            val cooldownExpired = Instant.now().isAfter(cooldownData.cooldownTimestamp)
            if (cooldownExpired) {
                CooldownExpiredEvent.EVENT.invoker().cooldownExpired(cooldownData, server)
            }
            cooldownExpired
        }
    }

    fun notifyCooldown(playerToNotify: ServerPlayer, cooldownData: CooldownData) {
        assert (getPlayerCooldown(playerToNotify.uuid, cooldownData.commandOnCooldown) == null) {
            "Tried to notify ${playerToNotify.name} of their \"/${cooldownData.commandOnCooldown.getCommandNameFromType()}\" cooldown, but they weren't on cooldown! Bad logic."
        }

        val formattedDurationText = Duration.between(Instant.now(), cooldownData.cooldownTimestamp).run {
            when {
                this > Duration.ofDays(1) -> "${this.toDays()}d ${this.toHoursPart()}h ${this.toMinutesPart()}m"
                this > Duration.ofHours(1) -> "${this.toHours()}h ${this.toMinutesPart()}m ${this.toSecondsPart()}s"
                this > Duration.ofMinutes(1) -> "${this.toMinutes()}m ${this.toSecondsPart()}s"
                this > Duration.ofSeconds(10) -> "${this.toSeconds()}s"
                this > Duration.ofSeconds(1) -> "${this.toSeconds()}.${this.toMillisPart()}s"
                else -> "${this.toMillis()}ms"
            }
        }

        playerToNotify.sendSystemMessage(Component.literal(MsgFmt.fmt(
            Messages.COMMAND_ON_COOLDOWN_MESSAGE.get(), mapOf(
                "command_used" to cooldownData.commandOnCooldown.getCommandNameFromType(),
                "time_remaining" to formattedDurationText
            )
        )))
    }

    fun getPlayerCooldown(playerUUID: UUID, commandType: CommandType): CooldownData? {
        return cooldowns.firstOrNull { cooldown ->
            cooldown.playerCooldownUUID == playerUUID &&
            cooldown.commandOnCooldown == commandType
        }
    }

    fun onCooldownExpired(cooldownData: CooldownData, server: MinecraftServer): EventResult {
        val cooledDownPlayer: ServerPlayer? = server.playerList.players.firstOrNull { player -> player.uuid == cooldownData.playerCooldownUUID }

        if (cooledDownPlayer == null) {
            return EventResult.pass()
        }

        return EventResult.pass()
    }
}
