package net.superricky.tpaplusplus.requests;

import net.minecraft.server.level.ServerPlayer;

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

public class RequestHelper {
    private static final Set<Request> requestSet = ConcurrentHashMap.newKeySet();

    public static Set<Request> getRequestSet() {
        return requestSet;
    }

    public static boolean isPlayerIdentical(ServerPlayer player1, ServerPlayer player2) {
        return player1.getUUID().equals(player2.getUUID());
    }

    public static void clearRequestSet() {
        requestSet.clear();
    }

    public static boolean teleportRequestExists(Request requestToFind) {
        for (Request request : requestSet) {
            if (isPlayerIdentical(requestToFind.getSender(), request.getSender())
                    && isPlayerIdentical(requestToFind.getReceiver(), request.getReceiver())) {
                return true;
            }
        }

        return false;
    }

    public static boolean alreadySentTeleportRequest(ServerPlayer sender, ServerPlayer receiver) {
        for (Request request : requestSet) {
            if (isPlayerIdentical(sender, request.getSender())
                    && isPlayerIdentical(receiver, request.getReceiver()))
                return true;
        }
        return false;
    }

    public static void teleport(Request request) {
        ServerPlayer sender = request.getSender();
        ServerPlayer receiver = request.getReceiver();

        // /tpahere
        if (request.isHereRequest()) {
            receiver.teleportTo(sender.serverLevel(), sender.getX(), sender.getY(), sender.getZ(), sender.getYRot(), sender.getXRot());
        }

        // /tpa
        sender.teleportTo(receiver.serverLevel(), receiver.getX(), receiver.getY(), receiver.getZ(), receiver.getYRot(), receiver.getXRot());
    }

    private RequestHelper() {
    }
}
