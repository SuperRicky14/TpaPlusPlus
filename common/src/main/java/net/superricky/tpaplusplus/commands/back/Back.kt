package net.superricky.tpaplusplus.commands.back;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.cooldown.CommandType;
import net.superricky.tpaplusplus.cooldown.CooldownData;
import net.superricky.tpaplusplus.cooldown.CooldownManager;
import org.jetbrains.annotations.Nullable;

import java.time.Duration;
import java.util.Objects;

public class Back {
    private Back() {
    }

    public static void teleportToLatestDeath(ServerPlayer executor) {

        if (Boolean.FALSE.equals(Config.BACK_COMMAND_ENABLED.get())) {
            executor.sendSystemMessage(Component.literal(Messages.ERR_BACK_COMMAND_DISABLED.get()));
            return;
        }

        @Nullable LevelBoundVec3 deathPosition = DeathHelper.getPlayerDeathCoordinates().get(executor.getUUID());

        // Protect against NullPointerException
        if (Objects.isNull(deathPosition)) {
            executor.sendSystemMessage(Component.literal(Messages.ERR_DEATH_LOC_NOT_FOUND.get()));
            return;
        }

        CooldownData cooldown;
        if ((cooldown = CooldownManager.INSTANCE.getPlayerCooldown(executor.getUUID(), CommandType.BACK)) != null) {
            CooldownManager.INSTANCE.notifyCooldown(executor, cooldown);
            return;
        }

        if (Config.BACK_COOLDOWN.get() > 0) // Check if cooldown is enabled
            CooldownManager.INSTANCE.scheduleCooldown(executor.getUUID(), Duration.ofSeconds(Config.BACK_COOLDOWN.get()), CommandType.BACK);

        absoluteTeleportToLatestDeath(executor, deathPosition);
    }

    public static void absoluteTeleportToLatestDeath(ServerPlayer executor, LevelBoundVec3 deathPosition) {
        executor.sendSystemMessage(Component.literal(Messages.DEATH_BEING_TELEPORTED.get()));

        teleportToLastPosition(executor, deathPosition);
        DeathHelper.removePlayerDeathCoordinates(executor.getUUID());

        executor.sendSystemMessage(Component.literal(Messages.DEATH_TELEPORTED.get()));
    }

    public static void teleportToLastPosition(ServerPlayer executor, LevelBoundVec3 deathPosition) {
        executor.teleportTo(deathPosition.serverLevel(), deathPosition.x, deathPosition.y, deathPosition.z, executor.getYRot(), executor.getXRot());
    }
}
