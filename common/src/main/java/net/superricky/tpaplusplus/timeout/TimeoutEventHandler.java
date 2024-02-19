package net.superricky.tpaplusplus.timeout;

import dev.architectury.event.EventResult;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.requests.Request;
import net.superricky.tpaplusplus.requests.RequestHelper;

public class TimeoutEventHandler {
    /**
     * Our custom event.
     * This event is triggered once the timer of a teleport request reaches 0, notifying all members that were affected.
     */
    public static EventResult onTimeoutEvent(Request request) {
        /* Check if the request has not been accepted or denied, so you don't print timeout messages multiple times.
         * UPDATE: This now ACTUALLY prevents printing timeout messages multiple times, because now the check is inverted (see RequestHelper#alreadySentTeleportRequest),
         * since before we were only displaying your timeout message IF the timeout message did NOT expire, which you can imagine that caused problems.
         */
        if (Boolean.FALSE.equals(RequestHelper.teleportRequestExists(request))) return EventResult.pass();

        ServerPlayer receiver = request.getReceiver();
        ServerPlayer sender = request.getSender();

        if (request.isHereRequest()) {
            sender.sendSystemMessage(Component.literal(String.format(Messages.SENDER_TPAHERE_TIMEOUT.get(), receiver.getDisplayName().getString())));
            receiver.sendSystemMessage(Component.literal(String.format(Messages.RECEIVER_TPAHERE_TIMEOUT.get(), sender.getDisplayName().getString())));
            RequestHelper.getRequestSet().remove(request);
            return EventResult.pass();
        }

        sender.sendSystemMessage(Component.literal(String.format(Messages.SENDER_TPA_TIMEOUT.get(), receiver.getDisplayName().getString())));
        receiver.sendSystemMessage(Component.literal(String.format(Messages.RECEIVER_TPA_TIMEOUT.get(), sender.getDisplayName().getString())));
        RequestHelper.getRequestSet().remove(request);
        return EventResult.pass();
    }

    private TimeoutEventHandler() {
    }
}
