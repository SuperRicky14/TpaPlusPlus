package net.superricky.tpaplusplus;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.teleport.Teleport;
import net.superricky.tpaplusplus.teleport.TeleportHere;
import net.superricky.tpaplusplus.teleport.TeleportTo;

import javax.annotation.Nullable;
import java.util.Map;
import java.util.Objects;

public class TeleportManager {
    private static boolean alreadySentTeleportRequest(Teleport teleportRequest) {
        /*
         * Returns:
         * (false) if there is no existing teleport request in the hashmap
         * (true) if there is already a teleport request in the hashmap
         */
        return Main.teleportRequests.containsKey(teleportRequest);
    }

    public static void sendTeleportTo(TeleportTo teleportToRequest) {
        /* The reason why we first add the @Nullable annotator, then catch it here, so we can add our own message instead of getting:
         * "An unexpected error occurred" in our output
         */
        if (Objects.isNull(teleportToRequest)) throw new IllegalArgumentException("Teleport-TO request is null!");

        ServerPlayer executor = teleportToRequest.executor();
        ServerPlayer teleported = teleportToRequest.teleported();

        // Protect against NullPointerException
        if (Objects.isNull(executor) || Objects.isNull(teleported)) throw new IllegalArgumentException("Received null ServerPlayer object(s)");

        // Notify and return if there already is a teleport request in the HashMap
        if (alreadySentTeleportRequest(teleportToRequest)) {
            executor.sendSystemMessage(Component.literal("§cYou already sent a teleport request to this player!"));
            executor.sendSystemMessage(Component.literal("§6Run §c/tpacancel §6to cancel that request!"));
            return;
        }

        Main.teleportRequests.put(teleportToRequest, Config.TPA_TIMEOUT_IN_SECONDS.get() * 20);

        executor.sendSystemMessage(Component.literal("§6Sent teleport request to §c" + teleported.getDisplayName().getString()));
        teleported.sendSystemMessage(Component.literal("§6Teleport request received from §c" + executor.getDisplayName().getString()));
    }

    public static void sendTeleportHere(TeleportHere teleportHereRequest) {
        /* The reason why we first add the @Nullable annotator, then catch it here, so we can add our own message instead of getting:
         * "An unexpected error occurred" in our output
         */
        if (Objects.isNull(teleportHereRequest)) throw new IllegalArgumentException("Teleport-HERE request is null!");

        ServerPlayer executor = teleportHereRequest.executor();
        ServerPlayer teleported = teleportHereRequest.teleported();

        // Protect against NullPointerException
        if (Objects.isNull(executor) || Objects.isNull(teleported)) throw new IllegalArgumentException("Received null ServerPlayer object(s)");

        // Notify and return if there already is a teleport request in the HashMap
        if (alreadySentTeleportRequest(teleportHereRequest)) {
            executor.sendSystemMessage(Component.literal("§cYou already sent a teleport §fhere §crequest to this player!"));
            executor.sendSystemMessage(Component.literal("§6Run §c/tpacancel §6to cancel that request!"));
            return;
        }

        Main.teleportRequests.put(teleportHereRequest, Config.TPA_TIMEOUT_IN_SECONDS.get() * 20);

        executor.sendSystemMessage(Component.literal("§6Sent teleport §fhere §6request to §c" + teleported.getDisplayName().getString()));
        teleported.sendSystemMessage(Component.literal("§6Teleport §fhere §6request received from §c" + executor.getDisplayName().getString()));
    }

    public static void acceptTeleportRequest(@Nullable Teleport teleportRequest) {
        /* The reason why we first add the @Nullable annotator, then catch it here, so we can add our own message instead of getting:
         * "An unexpected error occurred" in our output
         */
        if (Objects.isNull(teleportRequest)) throw new IllegalArgumentException("Teleport request is null!");

        ServerPlayer executor = teleportRequest.executor();
        ServerPlayer teleported = teleportRequest.teleported();

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

    public static void denyTeleportRequest(@Nullable Teleport teleportRequest) {
        /* The reason why we first add the @Nullable annotator, then catch it here, so we can add our own message instead of getting:
         * "An unexpected error occurred" in our output
         */
        if (Objects.isNull(teleportRequest)) throw new IllegalArgumentException("Teleport request is null!");

        ServerPlayer executor = teleportRequest.executor();
        ServerPlayer teleported = teleportRequest.teleported();

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

    public static void cancelTeleportRequest(@Nullable Teleport teleportRequest) {
        /* The reason why we first add the @Nullable annotator, then catch it here, so we can add our own message instead of getting:
         * "An unexpected error occurred" in our output
         */
        if (Objects.isNull(teleportRequest)) throw new IllegalArgumentException("Teleport request is null!");

        ServerPlayer executor = teleportRequest.executor();
        ServerPlayer teleported = teleportRequest.teleported();

        // Protect against NullPointerException
        if (Objects.isNull(executor) || Objects.isNull(teleported)) throw new IllegalArgumentException("Received null ServerPlayer object(s)");

        if (teleportRequest instanceof TeleportTo) {
            executor.sendSystemMessage(Component.literal("§6Denied teleport §6request for §c" + teleported.getDisplayName().getString()));
            teleported.sendSystemMessage(Component.literal("§6Your teleport §6request from §c" + executor.getDisplayName().getString() + " §6was cancelled!"));
        } else if (teleportRequest instanceof TeleportHere) {
            executor.sendSystemMessage(Component.literal("§6Cancelled teleport §fhere §6request for §c" + teleported.getDisplayName().getString()));
            teleported.sendSystemMessage(Component.literal("§6Your teleport §fhere §6request from §c" + executor.getDisplayName().getString() + " §6was cancelled!"));
        }

        Main.teleportRequests.remove(teleportRequest);
    }

    // Helper Methods
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

    private static void teleportPlayerTo(ServerPlayer executor, ServerPlayer teleported) {
        executor.teleportTo(teleported.serverLevel(), teleported.getX(), teleported.getY(), teleported.getZ(), teleported.getYRot(), teleported.getXRot());
    }

    private static void teleportPlayerHere(ServerPlayer executor, ServerPlayer teleported) {
        teleported.teleportTo(executor.serverLevel(), executor.getX(), executor.getY(), executor.getZ(), executor.getYRot(), executor.getXRot());
    }
}
