package net.superricky.tpaplusplus.commands.back;

import dev.architectury.event.EventResult;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;

import java.util.HashMap;
import java.util.Map;

public class DeathHelper {
    private static final Map<ServerPlayer, LevelBoundVec3> playerDeathCoordinates = new HashMap<>();

    public static Map<ServerPlayer, LevelBoundVec3> getPlayerDeathCoordinates() {
        return playerDeathCoordinates;
    }

    /**
     * Triggered when an entity dies.
     * Here we check if the entity is a player, if so, then we log its death position, and remove the old one.
     * If it is not, then we skip it.
     * The reason why we still run this code even when it is disabled,
     * is so if a player disables or enables "/back" during runtime,
     * it will actually still keep this logged.
     * I might add this as a config option later on if you are that concerned about performance.
     */
    public static EventResult onDeath(LivingEntity deadEntity) {
        // This check will return if the dead entity isn't a player, but if it is, then it will cast it into a ServerPlayer variable called playerEntity.
        if (!(deadEntity instanceof ServerPlayer playerEntity)) {
            // Return, since the deadEntity isn't a ServerPlayer
            return EventResult.pass();
        }

        // Get the death position as a net.minecraft.world.phys Vec3 object
        LevelBoundVec3 deathPosition = new LevelBoundVec3(playerEntity.serverLevel(), playerEntity.getX(), playerEntity.getY(), playerEntity.getZ());

        // Remove old playerDeathCoordinate if present.
        playerDeathCoordinates.remove(playerEntity);

        // Add this playerDeathCoordinate to the map.
        playerDeathCoordinates.put(playerEntity, deathPosition);

        return EventResult.pass();
    }

    public static void clearDeathCoordinates() {
        playerDeathCoordinates.clear();
    }
}
