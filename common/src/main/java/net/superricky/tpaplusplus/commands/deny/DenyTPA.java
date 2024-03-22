package net.superricky.tpaplusplus.commands.deny;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.requests.Request;
import net.superricky.tpaplusplus.requests.RequestGrabUtil;
import net.superricky.tpaplusplus.requests.RequestHelper;
import net.superricky.tpaplusplus.windupcooldown.cooldown.AsyncCooldownHelper;
import net.superricky.tpaplusplus.windupcooldown.cooldown.CooldownData;
import net.superricky.tpaplusplus.windupcooldown.windup.AsyncWindup;
import net.superricky.tpaplusplus.windupcooldown.CommandType;
import net.superricky.tpaplusplus.windupcooldown.windup.WindupData;

import java.util.Objects;

public class DenyTPA {
    // Deny command is run by the receiver, hence why it's in the receiver's point of view.
    private static void denyFunctionality(Request request, ServerPlayer receiver) {
        // run the notify cooldown function and return if its false, to stop the player from denying the request.
        if (!AsyncCooldownHelper.notifyAndCheckCooldown(receiver, receiver.getUUID(), CommandType.DENY)) return;

        if (Objects.isNull(request)) {
            receiver.sendSystemMessage(Component.literal(Messages.ERR_REQUEST_NOT_FOUND.get()));
            return;
        }

        if (Boolean.FALSE.equals(Config.ONLY_START_COOLDOWN_ON_COMMAND_SUCCESS.get()))
            AsyncCooldownHelper.postCooldown(receiver.getUUID(), CommandType.DENY, Config.DENY_COOLDOWN.get());

        if (Config.DENY_WINDUP.get() == 0) {
            absoluteDeny(request);
        } else {
            AsyncWindup.schedule(new WindupData(request, Config.DENY_WINDUP.get(), receiver.getX(), receiver.getY(), receiver.getZ(), CommandType.DENY, new ServerPlayer[]{receiver}));
        }
    }

    public static void absoluteDeny(Request request) {
        request.getReceiver().sendSystemMessage(Component.literal(String.format(Messages.RECEIVER_DENIES_TPA.get(), request.getSender().getName().getString())));
        request.getSender().sendSystemMessage(Component.literal(String.format(Messages.SENDER_GOT_DENIED_TPA.get(), request.getReceiver().getName().getString())));

        RequestHelper.getRequestSet().remove(request);

        if (Boolean.TRUE.equals(Config.ONLY_START_COOLDOWN_ON_COMMAND_SUCCESS.get()))
            AsyncCooldownHelper.postCooldown(request.getReceiver().getUUID(), CommandType.DENY, Config.DENY_COOLDOWN.get());

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
