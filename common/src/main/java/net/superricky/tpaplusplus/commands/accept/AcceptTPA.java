package net.superricky.tpaplusplus.commands.accept;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.requests.Request;
import net.superricky.tpaplusplus.requests.RequestGrabUtil;
import net.superricky.tpaplusplus.requests.RequestHelper;
import net.superricky.tpaplusplus.util.MsgFmt;
import net.superricky.tpaplusplus.windupcooldown.cooldown.CommandType;
import net.superricky.tpaplusplus.windupcooldown.cooldown.CooldownData;
import net.superricky.tpaplusplus.windupcooldown.cooldown.CooldownManager;
import net.superricky.tpaplusplus.windupcooldown.windup.AsyncWindup;
import net.superricky.tpaplusplus.windupcooldown.windup.impl.AcceptWindup;

import java.time.Duration;
import java.util.Map;
import java.util.Objects;

public class AcceptTPA {
    // Accept command is run by the sender, hence why it's in the sender's point of view.
    public static void acceptFunctionality(Request request, ServerPlayer receiver) {
        if (Objects.isNull(request)) {
            receiver.sendSystemMessage(Component.literal(Messages.ERR_REQUEST_NOT_FOUND.get()));
            return;
        }

        CooldownData cooldown;
        if ((cooldown = CooldownManager.INSTANCE.getPlayerCooldown(receiver.getUUID(), CommandType.ACCEPT)) != null) {
            CooldownManager.INSTANCE.notifyCooldown(receiver, cooldown);
            return;
        }

        if (Config.ACCEPT_COOLDOWN.get() > 0) // Check if cooldown is enabled
            CooldownManager.INSTANCE.scheduleCooldown(receiver.getUUID(), Duration.ofSeconds(Config.ACCEPT_COOLDOWN.get()), CommandType.ACCEPT);

        if (Config.ACCEPT_WINDUP.get() == 0) {
            absoluteAcceptFunctionality(request, receiver);
        } else {
            AsyncWindup.INSTANCE.schedule(new AcceptWindup(request));
        }
    }

    public static void absoluteAcceptFunctionality(Request request, ServerPlayer receiver) {
        receiver.sendSystemMessage(Component.literal(MsgFmt.fmt(Messages.RECEIVER_ACCEPTS_TPA.get(), Map.of("senders_name", request.getSender().getName().getString()))));
        request.getSender().sendSystemMessage(Component.literal(MsgFmt.fmt(Messages.SENDER_GOT_ACCEPTED_TPA.get(), Map.of("receivers_name", request.getReceiver().getName().getString()))));

        RequestHelper.teleport(request);

        RequestHelper.getRequestSet().remove(request);
    }

    public static void acceptTeleportRequest(ServerPlayer receiver) {
        Request request = RequestGrabUtil.getReceiverRequest(receiver);
        acceptFunctionality(request, receiver);
    }

    public static void acceptTeleportRequest(ServerPlayer receiver, ServerPlayer sender) {
        Request request = RequestGrabUtil.getReceiverRequest(receiver, sender);
        acceptFunctionality(request, receiver);
    }

    private AcceptTPA() {
    }
}
