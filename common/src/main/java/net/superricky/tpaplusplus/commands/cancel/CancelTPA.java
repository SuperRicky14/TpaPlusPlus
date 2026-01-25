package net.superricky.tpaplusplus.commands.cancel;

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

public class CancelTPA {
    // Cancel command is run by the sender, hence why it's in the sender's point of view.
    private static void cancelFunctionality(Request request, ServerPlayer sender) {
        if (Objects.isNull(request)) {
            sender.sendSystemMessage(Component.literal(Messages.ERR_REQUEST_NOT_FOUND.get()));
            return;
        }

        CooldownData cooldown;
        if ((cooldown = CooldownManager.INSTANCE.getPlayerCooldown(sender.getUUID(), CommandType.CANCEL)) != null) {
            CooldownManager.INSTANCE.notifyCooldown(sender, cooldown);
            return;
        }

        if (Config.CANCEL_COOLDOWN.get() > 0) // Check if cooldown is enabled
            CooldownManager.INSTANCE.scheduleCooldown(sender.getUUID(), Duration.ofSeconds(Config.CANCEL_COOLDOWN.get()), CommandType.CANCEL);

        absoluteCancel(request);
    }

    public static void absoluteCancel(Request request) {
        request.getSender().sendSystemMessage(Component.literal(MsgFmtKt.template(Messages.SENDER_CANCELS_TPA.get(), Map.of("cancelled_tpa_recipient", request.getReceiver().getName().getString()))));
        request.getReceiver().sendSystemMessage(Component.literal(MsgFmtKt.template(Messages.RECEIVER_GOT_CANCELLED_TPA.get(), Map.of("cancelling_sender_of_tpa", request.getSender().getName().getString()))));

        RequestHelper.getRequestSet().remove(request);
    }

    public static void cancelTeleportRequest(ServerPlayer sender) {
        Request request = RequestGrabUtil.getSenderRequest(sender);
        cancelFunctionality(request, sender);
    }

    public static void cancelTeleportRequest(ServerPlayer sender, ServerPlayer receiver) {
        Request request = RequestGrabUtil.getSenderRequest(sender, receiver);
        cancelFunctionality(request, sender);
    }

    private CancelTPA() {
    }
}
