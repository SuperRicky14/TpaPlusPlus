package net.superricky.tpaplusplus.util;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.Config;
import net.superricky.tpaplusplus.Messages;

import javax.annotation.Nullable;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class DeathManager {
    public static final Map<ServerPlayer, DeathPos> playerDeathCoordinates = new HashMap<>();

    public static void teleportToLatestDeath(ServerPlayer executor) {
        if (!Config.BACK_COMMAND_ENABLED.get()) {
            executor.sendSystemMessage(Component.literal(Messages.ERR_BACK_COMMAND_DISABLED.get()));
            return;
        }

        @Nullable DeathPos deathPosition = playerDeathCoordinates.get(executor);

        // Protect against NullPointerException
        if (Objects.isNull(deathPosition)) {
            executor.sendSystemMessage(Component.literal(Messages.ERR_DEATH_LOC_NOT_FOUND.get()));
            return;
        }

        executor.sendSystemMessage(Component.literal(Messages.DEATH_BEING_TELEPORTED.get()));

        teleportToLastPosition(executor, deathPosition); // Teleport the player to their last position!
        playerDeathCoordinates.remove(executor); // Remove the player from the death coordinates afterward.

        executor.sendSystemMessage(Component.literal(Messages.DEATH_TELEPORTED.get()));
    }

    public static void teleportToLastPosition(@Nullable ServerPlayer executor, @Nullable DeathPos deathPosition) {
        executor.teleportTo(deathPosition.getDeathLevel(), deathPosition.x, deathPosition.y, deathPosition.z, executor.getYRot(), executor.getXRot());
    }
}
