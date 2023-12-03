package net.superricky.tpaplusplus;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.teleport.Teleport;
import net.superricky.tpaplusplus.teleport.TeleportHere;
import net.superricky.tpaplusplus.teleport.TeleportTo;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;

import javax.annotation.Nullable;
import java.util.Map;
import java.util.Objects;

public class TeleportManager {
    public static void sendTeleportTo(TeleportTo teleportToRequest) {
        ServerPlayer executor = teleportToRequest.getExecutor();
        ServerPlayer teleported = teleportToRequest.getTeleported();

        // Protect against NullPointerException
        if (Objects.isNull(executor) || Objects.isNull(teleported)) throw new IllegalArgumentException("Received null ServerPlayer object(s)");

        Main.teleportRequests.put(teleportToRequest, Main.teleportRequestTimeoutTime);

        executor.sendSystemMessage(Component.literal("§6Sent teleport request to §c" + teleported.getDisplayName().getString()));
        teleported.sendSystemMessage(Component.literal("§6Teleport request received from §c" + executor.getDisplayName().getString()));
    }

    public static void sendTeleportHere(TeleportHere teleportHereRequest) {
        ServerPlayer executor = teleportHereRequest.getExecutor();
        ServerPlayer teleported = teleportHereRequest.getTeleported();

        // Protect against NullPointerException
        if (Objects.isNull(executor) || Objects.isNull(teleported)) throw new IllegalArgumentException("Received null ServerPlayer object(s)");

        Main.teleportRequests.put(teleportHereRequest, Main.teleportRequestTimeoutTime);

        executor.sendSystemMessage(Component.literal("§6Sent teleport §fhere §6request to §c" + teleported.getDisplayName().getString()));
        teleported.sendSystemMessage(Component.literal("§6Teleport §fhere §6request received from §c" + executor.getDisplayName().getString()));
    }

    public static void acceptTeleportRequest(Teleport teleportRequest) {
        ServerPlayer executor = teleportRequest.getExecutor();
        ServerPlayer teleported = teleportRequest.getTeleported();

        // Protect against NullPointerException
        if (Objects.isNull(executor) || Objects.isNull(teleported)) throw new IllegalArgumentException("Received null ServerPlayer object(s)");

        if (teleportRequest instanceof TeleportTo) {
            teleportPlayerTo(executor, teleported);

            executor.sendSystemMessage(Component.literal("§6Your teleport §6request for §c" + teleported.getDisplayName().getString() + " §6was accepted!"));
            teleported.sendSystemMessage(Component.literal("§6Accepted teleport §6request from §c" + executor.getDisplayName().getString()));


        } else if (teleportRequest instanceof TeleportHere) {
            teleportPlayerHere(executor, teleported);

            executor.sendSystemMessage(Component.literal("§6Your teleport §fhere §6request for §c" + teleported.getDisplayName().getString() + " §6was accepted!"));
            teleported.sendSystemMessage(Component.literal("§6Accepted teleport §fhere §6request from §c" + executor.getDisplayName().getString()));
        }

        Main.teleportRequests.remove(teleportRequest);
    }

    public static void denyTeleportRequest(Teleport teleportRequest) {
        ServerPlayer executor = teleportRequest.getExecutor();
        ServerPlayer teleported = teleportRequest.getTeleported();

        // Protect against NullPointerException
        if (Objects.isNull(executor) || Objects.isNull(teleported)) throw new IllegalArgumentException("Received null ServerPlayer object(s)");

        if (teleportRequest instanceof TeleportTo) {
            executor.sendSystemMessage(Component.literal("§6Your teleport §6request for §c" + teleported.getDisplayName().getString() + " §6was denied!"));
            teleported.sendSystemMessage(Component.literal("§6Denied teleport §6request from §c" + executor.getDisplayName().getString()));


        } else if (teleportRequest instanceof TeleportHere) {
            executor.sendSystemMessage(Component.literal("§6Your teleport §fhere §6request for §c" + teleported.getDisplayName().getString() + " §6was denied!"));
            teleported.sendSystemMessage(Component.literal("§6Denied teleport §fhere §6request from §c" + executor.getDisplayName().getString()));
        }

        Main.teleportRequests.remove(teleportRequest);
    }

    public static void cancelTeleportRequest(Teleport teleportRequest) {
        ServerPlayer executor = teleportRequest.getExecutor();
        ServerPlayer teleported = teleportRequest.getTeleported();

        // Protect against NullPointerException
        if (Objects.isNull(executor) || Objects.isNull(teleported)) throw new IllegalArgumentException("Received null ServerPlayer object(s)");

        if (teleportRequest instanceof TeleportTo) {
            executor.sendSystemMessage(Component.literal("§6Denied teleport §6request for §c" + teleported.getDisplayName().getString()));
            teleported.sendSystemMessage(Component.literal("§6Your teleport §6request from §c" + executor.getDisplayName().getString() + " §6was cancelled!"));
        } else if (teleportRequest instanceof TeleportHere) {
            executor.sendSystemMessage(Component.literal("§Cancelled teleport §fhere §6request for §c" + teleported.getDisplayName().getString()));
            teleported.sendSystemMessage(Component.literal("§6Your teleport §fhere §6request from §c" + executor.getDisplayName().getString() + " §6was cancelled!"));
        }

        Main.teleportRequests.remove(teleportRequest);
    }


    // Helper Methods
    @Nullable
    public static Teleport getTeleportRequestByPlayers(ServerPlayer executor, ServerPlayer teleported) {
        for (Map.Entry<Teleport, Integer> entry : Main.teleportRequests.entrySet()) {
            Teleport teleport = entry.getKey();
            ServerPlayer teleportExecutor = teleport.getExecutor();
            ServerPlayer teleportTeleported = teleport.getTeleported();

            // Check if both executor and teleported match the provided players
            if (teleportExecutor == executor && teleportTeleported == teleported) {
                return teleport; // Return the matching teleport request
            }
        }

        return null; // Return null if no matching teleport request is found
    }
    @Nullable
    public static Teleport getLargestTeleportRequest(ServerPlayer executor) {
        Teleport largestTeleportRequest = null;
        int largestValue = Integer.MIN_VALUE; // Initialize with the smallest possible integer value

        // Loop through all entries in the teleportRequests map
        for (Map.Entry<Teleport, Integer> entry : Main.teleportRequests.entrySet()) {
            Teleport teleport = entry.getKey();
            ServerPlayer teleportExecutor = teleport.getExecutor();

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

    private static void teleportPlayerTo(ServerPlayer executor, ServerPlayer teleported) {
        executor.teleportTo(teleported.serverLevel(), teleported.getX(), teleported.getY(), teleported.getZ(), teleported.getYRot(), teleported.getXRot());
    }

    private static void teleportPlayerHere(ServerPlayer executor, ServerPlayer teleported) {
        teleported.teleportTo(executor.serverLevel(), executor.getX(), executor.getY(), executor.getZ(), executor.getYRot(), executor.getXRot());
    }
}
