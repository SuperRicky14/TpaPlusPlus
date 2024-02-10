package net.superricky.tpaplusplus.util.manager;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.util.LevelBoundVec3;
import net.superricky.tpaplusplus.util.configuration.Config;
import net.superricky.tpaplusplus.util.configuration.Messages;
import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class DeathManager {
    static final Map<ServerPlayer, LevelBoundVec3> playerDeathCoordinates = new HashMap<>();

    private DeathManager() {
    }

    public static void teleportToLatestDeath(ServerPlayer executor) {
        if (Boolean.FALSE.equals(Config.BACK_COMMAND_ENABLED.get())) {
            executor.sendSystemMessage(Component.literal(Messages.ERR_BACK_COMMAND_DISABLED.get()));
            return;
        }

        @Nullable LevelBoundVec3 deathPosition = playerDeathCoordinates.get(executor);

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

    public static void teleportToLastPosition(ServerPlayer executor, LevelBoundVec3 deathPosition) {
        executor.teleportTo(deathPosition.serverLevel(), deathPosition.x, deathPosition.y, deathPosition.z, executor.getYRot(), executor.getXRot());
    }

    public static void clearDeathCoordinates() {
        playerDeathCoordinates.clear();
    }
}
