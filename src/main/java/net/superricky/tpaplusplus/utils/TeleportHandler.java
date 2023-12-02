package net.superricky.tpaplusplus.utils;

import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.Style;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.common.MinecraftForge;
import net.superricky.tpaplusplus.modevents.TeleportRequestAcceptEvent;
import net.superricky.tpaplusplus.modevents.TeleportRequestCancelledEvent;
import net.superricky.tpaplusplus.modevents.TeleportRequestDeniedEvent;

import java.util.HashMap;


public class TeleportHandler {
    private static TeleportHandler instance;
    // Store all TPA request
    private final HashMap<ServerPlayer, TPARequest> pendingRequests;

    // Settings:
    private static final int allowedDistanceMax = 2000;
    private static final int allowedDistanceMin = 250;
    private static final boolean allowInterDimensional = true;

    // Class constructor
    private TeleportHandler() {
        // Initialize hashmap
        pendingRequests = new HashMap<>();
    }

    // Get the instance of this class
    public static synchronized TeleportHandler getInstance() {
        if (instance == null) {
            instance = new TeleportHandler();
        }
        return instance;
    }

    // Generate accept | deny buttons
    private static void generateClickButtons(ServerPlayer receiver) {
        String acceptCommand = "/tpaaccept " + receiver.getDisplayName().getString();
        String denyCommand = "/tpadeny " + receiver.getDisplayName().getString();

        String clickableTextMessage = "[\"\",{\"text\":\"§a§lAccept§r\",\"clickEvent\":{\"action\":\"run_command\",\"value\":\"" +
                acceptCommand + "\"}},{\"text\":\" §d|§r \"},{\"text\":\"§c§lDeny§r\",\"clickEvent\":{\"action\":\"run_command\",\"value\":\"" +
                denyCommand + "\"}}]";

        receiver.sendSystemMessage(Component.Serializer.fromJson(clickableTextMessage));
    }

    // Send a TPA-TO request
    public void sendTPARequest(ServerPlayer sender, ServerPlayer receiver) {
        // Check if the player is already in the hashmap, if so return and send an error message
        if (!(getTPARequestFromMap(receiver) == null)) {
            sender.sendSystemMessage(Component.literal(
                    "§6You already sent a teleport request to §c" + receiver.getDisplayName().getString()
            ));
            // TODO: cancel button
            return;
        }

        TPARequest request = new TPARequest(sender, receiver, System.currentTimeMillis(), "teleport-to");
        pendingRequests.put(receiver, request);

        // Notify receiver about request
        receiver.sendSystemMessage(Component.literal("§c" + sender.getDisplayName().getString() + " §6wants to teleport to you!"));
        sender.sendSystemMessage(Component.literal("§6Sent teleport request to:§c" + receiver.getDisplayName().getString() + "§6!"));

        // Generate accept / deny buttons
        generateClickButtons(receiver);
    }

    // Send a TPA-HERE request
    public void sendTPARequestHere(ServerPlayer sender, ServerPlayer receiver) {
        // Check if the player is already in the hashmap, if so return and send an error message
        if (!(getTPARequestFromMap(receiver) == null)) {
            sender.sendSystemMessage(Component.literal(
                    "§6You already sent a teleport request to §c" + receiver.getDisplayName().getString()
            ));
            // TODO: cancel button
            return;
        }

        TPARequest request = new TPARequest(sender, receiver, System.currentTimeMillis(), "teleport-here");
        pendingRequests.put(receiver, request);

        // Notify receiver about request
        receiver.sendSystemMessage(Component.literal("§c" + sender.getDisplayName().getString() + " §6sent you a teleport-here request!"));
        sender.sendSystemMessage(Component.literal("§6Sent teleport-here request to:§c" + receiver.getDisplayName().getString() + "§6!"));

        // Generate accept / deny buttons
        generateClickButtons(receiver);
    }

    // Deny a specific TPA request
    public void denySpecifiedTPARequest(ServerPlayer receiver) {
        TPARequest request = getTPARequestFromMap(receiver);
        if (request != null && !request.accepted) {
            // Get the sender from the request
            ServerPlayer sender = request.sender;

            TeleportRequestDeniedEvent denyEvent = new TeleportRequestDeniedEvent(sender, receiver);
            MinecraftForge.EVENT_BUS.post(denyEvent);

            // Delete the request to stop memory leaks and bad accesses
            pendingRequests.remove(receiver, request);
        }
    }

    // Deny the most recent tpa request
    public void denyMostRecentTPARequest(ServerPlayer executor) {
        // Get the most recent request assigned to the executor
        // Loop through pendingRequests to find the most recent request for the executor
        TPARequest mostRecentRequest = null;
        long maxTimestamp = 0;
        for (TPARequest request : pendingRequests.values()) {
            if (request.receiver.equals(executor) && request.timestamp > maxTimestamp) {
                maxTimestamp = request.timestamp;
                mostRecentRequest = request;
            }
        }

        if (mostRecentRequest != null && !mostRecentRequest.accepted) {
            denySpecifiedTPARequest(executor); // Invoke your existing deny method
        } else {
            // Handle no pending request or already denied request for the executor
            executor.sendSystemMessage(Component.literal("§6No pending §cTPA request §6found to deny."));
        }
    }

    // Accept the specified TPA request
    public void acceptSpecifiedTPARequest(ServerPlayer receiver) {
        TPARequest request = getTPARequestFromMap(receiver);
        if (request != null && !request.accepted) {
            // Get the sender from the request
            ServerPlayer sender = request.sender;

            TeleportRequestAcceptEvent acceptEvent = new TeleportRequestAcceptEvent(sender, receiver);
            MinecraftForge.EVENT_BUS.post(acceptEvent);

            // Delete the request to stop memory leaks and bad accesses
            pendingRequests.remove(receiver, request);
        }
    }

    // accept the most recent TPA request
    public void acceptMostRecentTPARequest(ServerPlayer executor) {
        // Get the most recent request assigned to the executor
        // Loop through pendingRequests to find the most recent request for the executor
        TPARequest mostRecentRequest = null;
        long maxTimestamp = 0;
        for (TPARequest request : pendingRequests.values()) {
            if (request.receiver.equals(executor) && request.timestamp > maxTimestamp) {
                maxTimestamp = request.timestamp;
                mostRecentRequest = request;
            }
        }

        if (mostRecentRequest != null && !mostRecentRequest.accepted) {
            acceptSpecifiedTPARequest(executor); // Invoke your existing accept method
        } else {
            // Handle no pending request or already accepted request for the executor
            executor.sendSystemMessage(Component.literal("§6No pending §cTPA request §6found for you."));
        }
    }

    // Deny a specific TPA request
    public void cancelSpecifiedTPARequest(ServerPlayer receiver) {
        TPARequest request = getTPARequestFromMap(receiver);
        if (request != null && !request.accepted) {
            // Get the sender from the request
            ServerPlayer sender = request.sender;

            TeleportRequestCancelledEvent cancelledEvent = new TeleportRequestCancelledEvent(sender, receiver);
            MinecraftForge.EVENT_BUS.post(cancelledEvent);

            // Delete the request to stop memory leaks and bad accesses
            pendingRequests.remove(receiver, request);
        }
    }

    // Deny the most recent tpa request
    public void cancelMostRecentTPARequest(ServerPlayer executor) {
        // Get the most recent request assigned to the executor
        // Loop through pendingRequests to find the most recent request for the executor
        TPARequest mostRecentRequest = null;
        long maxTimestamp = 0;
        for (TPARequest request : pendingRequests.values()) {
            if (request.receiver.equals(executor) && request.timestamp > maxTimestamp) {
                maxTimestamp = request.timestamp;
                mostRecentRequest = request;
            }
        }

        if (mostRecentRequest != null && !mostRecentRequest.accepted) {
            cancelSpecifiedTPARequest(executor); // Invoke your existing cancel method
        } else {
            // Handle no pending request or already denied request for the executor
            executor.sendSystemMessage(Component.literal("§6No pending §cTPA request §6found to cancel."));
        }
    }

    public boolean teleportSenderToReceiver(ServerPlayer sender, ServerPlayer receiver) {
        BlockPos senderPos = sender.getOnPos();
        BlockPos receiverPos = receiver.getOnPos();

        ServerLevel receiverLevel = receiver.serverLevel();
        ServerLevel senderLevel = sender.serverLevel();

        sender.teleportTo(receiverLevel,
                receiverPos.getX(),
                receiverPos.getY() + 1,
                receiverPos.getZ(), 0, 0);
        return true;
    }

    public boolean teleportReceiverToSender(ServerPlayer sender, ServerPlayer receiver) {
        BlockPos senderPos = sender.getOnPos();
        BlockPos receiverPos = receiver.getOnPos();

        ServerLevel receiverLevel = receiver.serverLevel();
        ServerLevel senderLevel = sender.serverLevel();

        receiver.teleportTo(receiverLevel,
                senderPos.getX(),
                senderPos.getY() + 1,
                senderPos.getZ(), 0, 0);
        return true;
    }

    public TPARequest getTPARequestFromMap(ServerPlayer receiver) {
        return pendingRequests.get(receiver);
    }

    // Distance and dimension checks
    private static boolean isInSameDimension(ServerLevel receiverLevel, ServerLevel senderLevel) {
        if (receiverLevel.equals(senderLevel)) {
            return true;
        }

        return false;
    }

    private static boolean isInRange3D(BlockPos receiverPos, BlockPos senderPos) {
        int receiverX = receiverPos.getX();
        int receiverY = receiverPos.getY();
        int receiverZ = receiverPos.getZ();
        int senderX = receiverPos.getX();
        int senderY = receiverPos.getY();
        int senderZ = receiverPos.getZ();

        double distance = calculate3DDistance(receiverX, receiverY, receiverZ, senderX, senderY, senderZ);
        if (allowedDistanceMax > distance && allowedDistanceMin < distance) {
            return true;
        }

        return false;
    }

    private static boolean isInRange2D(BlockPos receiverPos, BlockPos senderPos) {
        int receiverX = receiverPos.getX();
        int receiverZ = receiverPos.getZ();
        int senderX = receiverPos.getX();
        int senderZ = receiverPos.getZ();

        double distance = calculate2DDistance(receiverX, receiverZ, senderX, senderZ);
        if (allowedDistanceMax > distance && allowedDistanceMin < distance) {
            return true;
        }

        return false;
    }

    private static double calculate3DDistance(double playerOneX, double playerOneY, double playerOneZ,
                                             double playerTwoX, double playerTwoY, double playerTwoZ) {
        return Math.sqrt(Math.pow(playerTwoX - playerOneX, 2) +
                Math.pow(playerTwoY - playerOneY, 2) +
                Math.pow(playerTwoZ - playerOneZ, 2));
    }

    private static double calculate2DDistance(double x1, double z1, double x2, double z2) {
        return Math.sqrt(Math.pow(x2 - x1, 2) + Math.pow(z2 - z1, 2));
    }
}
