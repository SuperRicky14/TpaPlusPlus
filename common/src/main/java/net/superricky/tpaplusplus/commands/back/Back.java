package net.superricky.tpaplusplus.commands.back;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.windupcooldown.CommandType;
import net.superricky.tpaplusplus.windupcooldown.cooldown.AsyncCooldownHelper;
import net.superricky.tpaplusplus.windupcooldown.cooldown.AsyncCooldownKt;
import net.superricky.tpaplusplus.windupcooldown.windup.AsyncWindup;
import net.superricky.tpaplusplus.windupcooldown.windup.WindupData;
import org.jetbrains.annotations.Nullable;

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

        if (AsyncCooldownHelper.checkCommandCooldownAndNotify(executor, executor.getUUID(), CommandType.BACK))
            return;

        if (Config.BACK_COOLDOWN.get() > 0) // Check if cooldown is enabled
            AsyncCooldownKt.scheduleCooldown(executor.getUUID(), Config.BACK_COOLDOWN.get(), CommandType.BACK);

        if (Config.BACK_WINDUP.get() == 0) {
            absoluteTeleportToLatestDeath(executor, deathPosition);
        } else {
            AsyncWindup.schedule(new WindupData(deathPosition, Config.BACK_WINDUP.get(), executor.getX(), executor.getY(), executor.getZ(), CommandType.BACK, new ServerPlayer[]{executor}));
        }
    }

    public static void absoluteTeleportToLatestDeath(ServerPlayer executor, LevelBoundVec3 deathPosition) {
        executor.sendSystemMessage(Component.literal(Messages.DEATH_BEING_TELEPORTED.get()));

        teleportToLastPosition(executor, deathPosition); // Teleport the player to their last position!
        DeathHelper.getPlayerDeathCoordinates().remove(executor.getUUID()); // Remove the player from the death coordinates afterward.

        executor.sendSystemMessage(Component.literal(Messages.DEATH_TELEPORTED.get()));
    }

    public static void teleportToLastPosition(ServerPlayer executor, LevelBoundVec3 deathPosition) {
        executor.teleportTo(deathPosition.serverLevel(), deathPosition.x, deathPosition.y, deathPosition.z, executor.getYRot(), executor.getXRot());
    }
}
