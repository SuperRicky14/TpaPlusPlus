package net.superricky.tpaplusplus.util.manager;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.util.Request;
import net.superricky.tpaplusplus.util.configuration.Config;
import net.superricky.tpaplusplus.util.configuration.Messages;
import net.superricky.tpaplusplus.util.configuration.formatters.MessageParser;
import net.superricky.tpaplusplus.util.limitations.LimitationManager;
import net.superricky.tpaplusplus.util.manager.saved.PlayerData;
import net.superricky.tpaplusplus.util.manager.saved.SaveDataManager;
import org.jetbrains.annotations.Nullable;

import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

public class RequestManager {
    static final Set<Request> requestSet = new HashSet<>();

    private RequestManager() {
    }

    public static boolean isPlayerIdentical(ServerPlayer player1, ServerPlayer player2) {
        return player1.getUUID().equals(player2.getUUID());
    }

    public static void clearRequestSet() {
        requestSet.clear();
    }

    public static boolean alreadySentTeleportRequest(Request request) {
        for (Request currentRequest : requestSet) {
            if (isPlayerIdentical(request.getSender(), currentRequest.getSender())
                    && isPlayerIdentical(request.getReceiver(), currentRequest.getReceiver())) {
                return true;
            }
        }
        return false;
    }

    // Deny command is run by the receiver, hence why it's in the receiver's point of view.
    private static void denyFunctionality(Request request, ServerPlayer receiver) {
        if (Objects.isNull(request)) {
            receiver.sendSystemMessage(Component.literal(Messages.ERR_REQUEST_NOT_FOUND.get()));
            return;
        }

        request.getReceiver().sendSystemMessage(Component.literal(String.format(Messages.RECEIVER_DENIES_TPA.get(), request.getSender().getName().getString())));
        request.getSender().sendSystemMessage(Component.literal(String.format(Messages.SENDER_GOT_DENIED_TPA.get(), request.getReceiver().getName().getString())));

        requestSet.remove(request);
    }
    public static void denyTeleportRequest(ServerPlayer receiver) {
        Request request = RequestGrabUtil.getReceiverRequest(receiver);
        denyFunctionality(request, receiver);
    }

    // Deny command is run by the receiver, hence why it's in the receiver's point of view.
    public static void denyTeleportRequest(ServerPlayer receiver, ServerPlayer sender) {
        Request request = RequestGrabUtil.getReceiverRequest(receiver, sender);
        denyFunctionality(request, receiver);
    }

    // Cancel command is run by the sender, hence why it's in the sender's point of view.
    private static void cancelFunctionality(Request request, ServerPlayer sender) {
        if (Objects.isNull(request)) {
            sender.sendSystemMessage(Component.literal(Messages.ERR_REQUEST_NOT_FOUND.get()));
            return;
        }

        request.getSender().sendSystemMessage(Component.literal(String.format(Messages.SENDER_CANCELS_TPA.get(), request.getReceiver().getName().getString())));
        request.getReceiver().sendSystemMessage(Component.literal(String.format(Messages.RECEIVER_GOT_CANCELLED_TPA.get(), request.getSender().getName().getString())));

        requestSet.remove(request);
    }
    public static void cancelTeleportRequest(ServerPlayer sender) {
        Request request = RequestGrabUtil.getSenderRequest(sender);
        cancelFunctionality(request, sender);
    }

    public static void cancelTeleportRequest(ServerPlayer sender, ServerPlayer receiver) {
        Request request = RequestGrabUtil.getSenderRequest(sender, receiver);
        cancelFunctionality(request, sender);
    }

    // Send command is run by the sender, hence why its in the sender's point of view
    public static void sendTeleportRequest(ServerPlayer sender, ServerPlayer receiver, boolean isHereRequest) {
        if (isPlayerIdentical(sender, receiver)) {
            sender.sendSystemMessage(Component.literal(Messages.ERR_NO_SELF_TELEPORT.get()));
            return;
        }

        if (PlayerBlockingManager.isPlayerBlocked(sender, receiver)) return; // Return if one of the players has blocked the other player.

        PlayerData receiverData = SaveDataManager.getPlayerData(receiver);

        if (Boolean.FALSE.equals(Objects.isNull(receiverData)) && receiverData.getTPToggle()) { // receiverData is not null && receiver TP toggle is enabled.
            sender.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.ERR_RECEIVER_TP_DISABLED.get(),
                    Map.of("receiverName", receiver.getName().getString()))));
            return;
        }

        if (Boolean.FALSE.equals(Config.ALLOW_TPTOGGLED_PLAYERS_TO_SEND_REQUESTS.get())) { // Allow TPToggled players to send requests is disabled in the config
            PlayerData senderData = SaveDataManager.getPlayerData(sender);

            if (Boolean.FALSE.equals(Objects.isNull(senderData)) && senderData.getTPToggle()) { // senderData is not null && sender TP toggle is enabled.
                sender.sendSystemMessage(Component.literal(Messages.ERR_SENDER_TP_DISABLED.get()));
                return;
            }
        }

        // run the notify function and return if it false, to stop the player from sending the request.
        if (!LimitationManager.notifyAndCheckAllowedToTeleport(sender, receiver, false)) return;

        Request request = new Request(sender, receiver, isHereRequest);

        requestSet.add(request);

        AsyncTaskManager.scheduleTeleportTimeout(request);

        if (isHereRequest) {
            sender.sendSystemMessage(Component.literal(String.format(Messages.SENDER_SENT_TPAHERE.get(), receiver.getName().getString())));
            receiver.sendSystemMessage(Component.literal(String.format(Messages.RECEIVER_GOT_TPAHERE.get(), sender.getName().getString())));
        } else {
            sender.sendSystemMessage(Component.literal(String.format(Messages.SENDER_SENT_TPA.get(), receiver.getName().getString())));
            receiver.sendSystemMessage(Component.literal(String.format(Messages.RECEIVER_GOT_TPA.get(), sender.getName().getString())));
        }
    }

    // Accept command is run by the sender, hence why it's in the sender's point of view.
    static void acceptFunctionality(Request request, ServerPlayer receiver, boolean absolute) {
        if (Objects.isNull(request)) {
            receiver.sendSystemMessage(Component.literal(Messages.ERR_REQUEST_NOT_FOUND.get()));
            return;
        }

        if (absolute || Config.TPA_ACCEPT_TIME_IN_SECONDS.get() == 0) {
            absoluteAcceptFunctionality(request, receiver);
        } else {
            AsyncTaskManager.startTPAAcceptCountdown(request);
        }
    }

    private static void absoluteAcceptFunctionality(Request request, ServerPlayer receiver) {
        receiver.sendSystemMessage(Component.literal(String.format(Messages.RECEIVER_ACCEPTS_TPA.get(), request.getSender().getName().getString())));
        request.getSender().sendSystemMessage(Component.literal(String.format(Messages.SENDER_GOT_ACCEPTED_TPA.get(), request.getSender().getName().getString())));

        teleport(request);

        requestSet.remove(request);
    }

    public static void acceptTeleportRequest(ServerPlayer receiver) {
        Request request = RequestGrabUtil.getReceiverRequest(receiver);
        acceptFunctionality(request, receiver, false);
    }

    public static void acceptTeleportRequest(ServerPlayer receiver, ServerPlayer sender) {
        Request request = RequestGrabUtil.getReceiverRequest(receiver, sender);
        acceptFunctionality(request, receiver, false);
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

    /**
     * A utility class used inside the request manager for grabbing teleport requests from the requestSet, based on the players point of view.
     * This is important as since we have commands that the receiver runs, that also have to grab the same teleport request that was sent by the sender,
     * and vice-versa, meaning that we have to use something like this since there is no one-size-fits-all solution here.
     */
    private static class RequestGrabUtil {
        @Nullable
        public static Request getSenderRequest(ServerPlayer sender) {
            for (Request request : RequestManager.requestSet) {
                if (isPlayerIdentical(request.getSender(), sender)) {
                    return request;
                }
            }
            return null;
        }

        @Nullable
        public static Request getSenderRequest(ServerPlayer sender, ServerPlayer receiver) {
            for (Request request : RequestManager.requestSet) {
                if (isPlayerIdentical(request.getSender(), sender) &&
                        isPlayerIdentical(request.getReceiver(), receiver)) {
                    return request;
                }
            }
            return null;
        }

        @Nullable
        public static Request getReceiverRequest(ServerPlayer receiver) {
            for (Request request : RequestManager.requestSet) {
                if (isPlayerIdentical(request.getReceiver(), receiver)) {
                    return request;
                }
            }
            return null;
        }

        @Nullable
        public static Request getReceiverRequest(ServerPlayer receiver, ServerPlayer sender) {
            for (Request request : RequestManager.requestSet) {
                if (isPlayerIdentical(request.getReceiver(), receiver) &&
                        isPlayerIdentical(request.getSender(), sender)) {
                    return request;
                }
            }
            return null;
        }
    }
}
