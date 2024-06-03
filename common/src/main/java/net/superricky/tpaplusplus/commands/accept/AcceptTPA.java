package net.superricky.tpaplusplus.commands.accept;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.TPAPlusPlus;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.requests.Request;
import net.superricky.tpaplusplus.requests.RequestGrabUtil;
import net.superricky.tpaplusplus.requests.RequestHelper;
import net.superricky.tpaplusplus.windupcooldown.CommandType;
import net.superricky.tpaplusplus.windupcooldown.cooldown.AsyncCooldownHelper;
import net.superricky.tpaplusplus.windupcooldown.cooldown.AsyncCooldownKt;
import net.superricky.tpaplusplus.windupcooldown.windup.AsyncWindup;
import net.superricky.tpaplusplus.windupcooldown.windup.WindupData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

import static fuzs.forgeconfigapiport.impl.ForgeConfigAPIPort.LOGGER;

public class AcceptTPA {
    private final static Logger LOGGER = LoggerFactory.getLogger(TPAPlusPlus.MOD_ID);
    // Accept command is run by the sender, hence why it's in the sender's point of view.
    public static void acceptFunctionality(Request request, ServerPlayer receiver) {
        if (Objects.isNull(request)) {
            receiver.sendSystemMessage(Component.literal(Messages.ERR_REQUEST_NOT_FOUND.get()));
            return;
        }

        if (AsyncCooldownHelper.checkCommandCooldownAndNotify(receiver, receiver.getUUID(), CommandType.ACCEPT))
            return;

        if (Config.ACCEPT_COOLDOWN.get() > 0) // Check if cooldown is enabled
            AsyncCooldownKt.scheduleCooldown(receiver.getUUID(), Config.ACCEPT_COOLDOWN.get(), CommandType.ACCEPT);

        if (Config.ACCEPT_WINDUP.get() == 0) {
            absoluteAcceptFunctionality(request, receiver);
        } else {
            AsyncWindup.schedule(new WindupData(request, Config.ACCEPT_WINDUP.get(), receiver.getX(), receiver.getY(), receiver.getZ(), CommandType.ACCEPT, new ServerPlayer[]{receiver}));
        }
    }

    public static void absoluteAcceptFunctionality(Request request, ServerPlayer receiver) {
        receiver.sendSystemMessage(Component.literal(String.format(Messages.RECEIVER_ACCEPTS_TPA.get(), request.getSender().getName().getString())));
        request.getSender().sendSystemMessage(Component.literal(String.format(Messages.SENDER_GOT_ACCEPTED_TPA.get(), request.getReceiver().getName().getString())));

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
