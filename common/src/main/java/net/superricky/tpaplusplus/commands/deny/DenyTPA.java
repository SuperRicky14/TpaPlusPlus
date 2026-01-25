package net.superricky.tpaplusplus.commands.deny;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.cooldown.CommandType;
import net.superricky.tpaplusplus.cooldown.CooldownData;
import net.superricky.tpaplusplus.cooldown.CooldownManager;
import net.superricky.tpaplusplus.requests.Request;
import net.superricky.tpaplusplus.requests.RequestGrabUtil;
import net.superricky.tpaplusplus.requests.RequestHelper;
import net.superricky.tpaplusplus.util.MsgFmtKt;

import java.time.Duration;
import java.util.Map;
import java.util.Objects;

public class DenyTPA {
    // Deny command is run by the receiver, hence why it's in the receiver's point of view.
    private static void denyFunctionality(Request request, ServerPlayer receiver) {
        if (Objects.isNull(request)) {
            receiver.sendSystemMessage(Component.literal(Messages.ERR_REQUEST_NOT_FOUND.get()));
            return;
        }

        CooldownData cooldown;
        if ((cooldown = CooldownManager.INSTANCE.getPlayerCooldown(receiver.getUUID(), CommandType.DENY)) != null) {
            CooldownManager.INSTANCE.notifyCooldown(receiver, cooldown);
            return;
        }

        if (Config.DENY_COOLDOWN.get() > 0) // Check if cooldown is enabled
            CooldownManager.INSTANCE.scheduleCooldown(receiver.getUUID(), Duration.ofSeconds(Config.DENY_COOLDOWN.get()), CommandType.DENY);

        absoluteDeny(request);
    }

    public static void absoluteDeny(Request request) {
        request.getReceiver().sendSystemMessage(Component.literal(MsgFmtKt.template(Messages.RECEIVER_DENIES_TPA.get(), Map.of("denied_sender_name", request.getSender().getName().getString()))));
        request.getSender().sendSystemMessage(Component.literal(MsgFmtKt.template(Messages.SENDER_GOT_DENIED_TPA.get(), Map.of("receiver_who_denied", request.getReceiver().getName().getString()))));

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
