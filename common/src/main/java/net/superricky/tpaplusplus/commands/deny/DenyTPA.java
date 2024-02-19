package net.superricky.tpaplusplus.commands.deny;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.requests.Request;
import net.superricky.tpaplusplus.requests.RequestGrabUtil;
import net.superricky.tpaplusplus.requests.RequestHelper;
import net.superricky.tpaplusplus.windup.AsyncScheduler;
import net.superricky.tpaplusplus.windup.CommandType;
import net.superricky.tpaplusplus.windup.WindupData;

import java.util.Objects;

public class DenyTPA {
    // Deny command is run by the receiver, hence why it's in the receiver's point of view.
    private static void denyFunctionality(Request request, ServerPlayer receiver) {
        if (Objects.isNull(request)) {
            receiver.sendSystemMessage(Component.literal(Messages.ERR_REQUEST_NOT_FOUND.get()));
            return;
        }

        if (Config.DENY_WINDUP.get() == 0) {
            absoluteDeny(request);
        } else {
            AsyncScheduler.schedule(new WindupData(request, Config.DENY_WINDUP.get(), receiver.getX(), receiver.getY(), receiver.getZ(), CommandType.DENY, new ServerPlayer[]{receiver}));
        }
    }

    public static void absoluteDeny(Request request) {
        request.getReceiver().sendSystemMessage(Component.literal(String.format(Messages.RECEIVER_DENIES_TPA.get(), request.getSender().getName().getString())));
        request.getSender().sendSystemMessage(Component.literal(String.format(Messages.SENDER_GOT_DENIED_TPA.get(), request.getReceiver().getName().getString())));

        RequestHelper.getRequestSet().remove(request);
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

    private DenyTPA() {
    }
}
