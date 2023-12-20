package net.superricky.tpaplusplus;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.teleport.Teleport;
import net.superricky.tpaplusplus.teleport.TeleportHere;
import net.superricky.tpaplusplus.teleport.TeleportTo;
import org.jetbrains.annotations.NotNull;

import javax.annotation.Nullable;
import java.util.Map;
import java.util.Objects;


/**
 * A bunch of utility / helper methods for working with teleport requests!
 */
public class TeleportManager {
    private TeleportManager() {}

    /**
     * Gets a teleport request by the executor and the to be teleported player
     * This just iterates through the list and checks if there is a teleport request with the same executor and teleported provided.
     * @param executor A ServerPlayer that is used to find the teleport request.
     * @return A ServerPlayer object or null if no teleport request was found.
     */
    @Nullable
    public static Teleport getTeleportRequestByPlayers(ServerPlayer executor, ServerPlayer teleported) {
        for (Map.Entry<Teleport, Integer> entry : Main.teleportRequests.entrySet()) {
            Teleport teleport = entry.getKey();
            ServerPlayer teleportExecutor = teleport.executor();
            ServerPlayer teleportTeleported = teleport.teleported();

            // Check if both executor and teleported match the provided players
            if (teleportExecutor == executor && teleportTeleported == teleported) {
                return teleport; // Return the matching teleport request
            } else {
                return null;
            }
        }

        return null; // Return null if no matching teleport request is found
    }

    /**
     * Gets a teleport request by the player who initialised the request
     * This function still checks for the old Integer which was used to track which teleport request is the latest, and how long it has left but that's stupid so we are deprecating it and replacing this function
     * @param executor A ServerPlayer that is used to find the teleport request
     * @return A ServerPlayer object or null if no teleport request was found.
     */
    @Nullable
    public static Teleport getLargestTeleportRequest(ServerPlayer executor) {
        Teleport largestTeleportRequest = null;
        int largestValue = Integer.MIN_VALUE; // Initialize with the smallest possible integer value

        // Loop through all entries in the teleportRequests map
        for (Map.Entry<Teleport, Integer> entry : Main.teleportRequests.entrySet()) {
            Teleport teleport = entry.getKey();
            ServerPlayer teleportExecutor = teleport.executor();

            // Check if the executor matches the provided 'executor'
            if (teleportExecutor == executor) {
                int value = entry.getValue();

                // Check if the value associated with the teleport is larger than the current largest value
                if (value > largestValue) {
                    largestValue = value;
                    largestTeleportRequest = teleport;
                }
            }
        }

        return largestTeleportRequest;
    }

    // Returns true if there is either a TeleportTo request for these two players already in the map, OR if there is a TeleportHere request already for these two players in the map
    // Useful if you want to prevent having a TeleportTo and TeleportHere request simultaneously in the map
    public static boolean anyTeleportRequestInMap(ServerPlayer executor, ServerPlayer teleported) {
        boolean wasFound = false;

        for (Map.Entry<Teleport, Integer> entry : Main.teleportRequests.entrySet()) {
            Teleport currentTeleportRequest = entry.getKey();
            if (currentTeleportRequest.executor().equals(executor)
                    && currentTeleportRequest.teleported().equals(teleported)) {
                wasFound = true;
                break;
            }
        }

        return wasFound;
    }

    public static boolean anyTeleportRequestInMap(Teleport teleport) {
        boolean wasFound = false;

        for (Map.Entry<Teleport, Integer> entry : Main.teleportRequests.entrySet()) {
            Teleport currentTeleportRequest = entry.getKey();
            if (currentTeleportRequest.executor().equals(teleport.executor())
                    && currentTeleportRequest.teleported().equals(teleport.teleported())) {
                wasFound = true;
                break;
            }
        }

        return wasFound;
    }
}