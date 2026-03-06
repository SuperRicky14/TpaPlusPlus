package net.superricky.tpaplusplus.requests;

import net.minecraft.server.level.ServerPlayer;
import org.jetbrains.annotations.Nullable;

import static net.superricky.tpaplusplus.requests.RequestHelper.isPlayerIdentical;

/**
 * A utility class used inside the request manager for grabbing teleport requests from the requestSet, based on the players point of view.
 * This is important as since we have commands that the receiver runs, that also have to grab the same teleport request that was sent by the sender,
 * and vice-versa, meaning that we have to use something like this since there is no one-size-fits-all solution here.
 */
public class RequestGrabUtil {
    @Nullable
    public static Request getSenderRequest(ServerPlayer sender) {
        for (Request request : RequestHelper.getRequestSet()) {
            if (isPlayerIdentical(request.getSender(), sender)) {
                return request;
            }
        }
        return null;
    }

    @Nullable
    public static Request getSenderRequest(ServerPlayer sender, ServerPlayer receiver) {
        for (Request request : RequestHelper.getRequestSet()) {
            if (isPlayerIdentical(request.getSender(), sender) &&
                    isPlayerIdentical(request.getReceiver(), receiver)) {
                return request;
            }
        }
        return null;
    }

    @Nullable
    public static Request getReceiverRequest(ServerPlayer receiver) {
        for (Request request : RequestHelper.getRequestSet()) {
            if (isPlayerIdentical(request.getReceiver(), receiver)) {
                return request;
            }
        }
        return null;
    }

    @Nullable
    public static Request getReceiverRequest(ServerPlayer receiver, ServerPlayer sender) {
        for (Request request : RequestHelper.getRequestSet()) {
            if (isPlayerIdentical(request.getReceiver(), receiver) &&
                    isPlayerIdentical(request.getSender(), sender)) {
                return request;
            }
        }
        return null;
    }

    private RequestGrabUtil() {
    }
}
