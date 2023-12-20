package net.superricky.tpaplusplus;

import java.util.Objects;
import javax.annotation.Nullable;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;

public final class DeathManager {
    public static void teleportToLatestDeath(@Nullable ServerPlayer executor, @Nullable Vec3 deathPosition) {
        // Protect against NullPointerException
        if (Objects.isNull(executor) || Objects.isNull(deathPosition)) throw new IllegalArgumentException("Executor and / or death position is null!");

        executor.sendSystemMessage(Component.literal("§6Teleporting you to your §clatest death §6position..."));

        teleportToLastPosition(executor, deathPosition); // Teleport the player to their last position!
        Main.playerDeathCoordinates.remove(executor); // Remove the player from the death coordinates afterward.

        executor.sendSystemMessage(Component.literal("§6You have been teleported!"));
    }

    private DeathManager() {} // hide the public constructor since everything is static

    @Nullable
    public static Vec3 getDeathPosition(ServerPlayer executor) {
        // No need for any fancy loops here! We already handle duplicates in our event!
        @Nullable
        Vec3 deathPosition = Main.playerDeathCoordinates.get(executor);

        return deathPosition;
    }

    public static void teleportToLastPosition(@Nullable ServerPlayer executor, @Nullable Vec3 deathPosition) {
        // Protect against NullPointerException
        if (Objects.isNull(executor) || Objects.isNull(deathPosition)) throw new IllegalArgumentException("Executor and / or death position is null!");

        double x = deathPosition.x;
        double y = deathPosition.y;
        double z = deathPosition.z;

        executor.teleportTo(x, y, z);
    }
}
