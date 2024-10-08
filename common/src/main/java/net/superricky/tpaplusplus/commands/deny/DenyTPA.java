package net.superricky.tpaplusplus.commands.deny;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.requests.Request;
import net.superricky.tpaplusplus.requests.RequestGrabUtil;
import net.superricky.tpaplusplus.requests.RequestHelper;
import net.superricky.tpaplusplus.util.MsgFmt;
import net.superricky.tpaplusplus.windupcooldown.cooldown.AsyncCooldownHelper;
import net.superricky.tpaplusplus.windupcooldown.cooldown.AsyncCooldownKt;
import net.superricky.tpaplusplus.windupcooldown.cooldown.CommandType;
import net.superricky.tpaplusplus.windupcooldown.windup.AsyncWindupKt;
import net.superricky.tpaplusplus.windupcooldown.windup.impl.DenyWindup;

import java.util.Map;
import java.util.Objects;

public class DenyTPA {
    // Deny command is run by the receiver, hence why it's in the receiver's point of view.
    private static void denyFunctionality(Request request, ServerPlayer receiver) {
        if (Objects.isNull(request)) {
            receiver.sendSystemMessage(Component.literal(Messages.ERR_REQUEST_NOT_FOUND.get()));
            return;
        }

        if (AsyncCooldownHelper.checkCommandCooldownAndNotify(receiver, receiver.getUUID(), CommandType.DENY))
            return;

        if (Config.DENY_COOLDOWN.get() > 0) // Check if cooldown is enabled
            AsyncCooldownKt.scheduleCooldown(receiver.getUUID(), Config.DENY_COOLDOWN.get(), CommandType.DENY);

        if (Config.DENY_WINDUP.get() == 0) {
            absoluteDeny(request);
        } else {
            AsyncWindupKt.schedule(new DenyWindup(request));
        }
    }

    public static void absoluteDeny(Request request) {
        request.getReceiver().sendSystemMessage(Component.literal(MsgFmt.fmt(Messages.RECEIVER_DENIES_TPA.get(), Map.of("denied_sender_name", request.getSender().getName().getString()))));
        request.getSender().sendSystemMessage(Component.literal(MsgFmt.fmt(Messages.SENDER_GOT_DENIED_TPA.get(), Map.of("receiver_who_denied", request.getReceiver().getName().getString()))));

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
