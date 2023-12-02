package net.superricky.tpaplusplus.utils;

import com.mojang.logging.LogUtils;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.superricky.tpaplusplus.modevents.TeleportRequestAcceptEvent;
import net.superricky.tpaplusplus.modevents.TeleportRequestCancelledEvent;
import net.superricky.tpaplusplus.modevents.TeleportRequestDeniedEvent;
import net.superricky.tpaplusplus.utils.TeleportHandler;
import org.slf4j.Logger;

public class EventHandler {
    private static final Logger LOGGER = LogUtils.getLogger();
    private static final TeleportHandler teleportHandler = TeleportHandler.getInstance();

    private static RequestType getTeleportRequestType(ServerPlayer sender, ServerPlayer receiver) {
        return teleportHandler.getTPARequestFromMap(sender, receiver).type;
    }

    @SubscribeEvent
    public static void onTPAAcceptEvent(TeleportRequestAcceptEvent event) {
        ServerPlayer sender = event.getSender();
        ServerPlayer receiver = event.getReceiver();

        // Run teleportation logic!
        RequestType tpaRequestType = getTeleportRequestType(event.getSender(), event.getReceiver());
        if (tpaRequestType == RequestType.teleport_from) {
            if (!(TeleportHandler.getInstance().teleportReceiverToSender(sender, receiver))) {
                event.setCanceled(true);
                return;
            }
            // Notify sender and receiver about acceptance
            sender.sendSystemMessage(Component.literal(
                    "§c" + receiver.getDisplayName().getString() + " §6accepted your TPA-HERE request!"
            ));

            receiver.sendSystemMessage(Component.literal(
                    "§6You accepted §c" + sender.getDisplayName().getString() + "§6's TPA-HERE request!"
            ));

        } else if (tpaRequestType == RequestType.teleport_to) {
            if (!(TeleportHandler.getInstance().teleportSenderToReceiver(sender, receiver))) {
                event.setCanceled(true);
                return;
            }
            // Notify sender and receiver about acceptance
            sender.sendSystemMessage(Component.literal(
                    "§c" + receiver.getDisplayName().getString() + " §6accepted your TPA request!"
            ));

            receiver.sendSystemMessage(Component.literal(
                    "§6You accepted §c" + sender.getDisplayName().getString() + "§6's TPA request!"
            ));
        } else {
            LOGGER.warn("Encountered TPARequest object in broken state!");
        }
    }

    @SubscribeEvent
    public static void onTPADenyEvent(TeleportRequestDeniedEvent event) {
        ServerPlayer sender = event.getSender();
        ServerPlayer receiver = event.getReceiver();

        // Notify sender and receiver about denial
        sender.sendSystemMessage(Component.literal(
                "§c" + receiver.getDisplayName().getString() + " §6denied your §cTPA request!"
        ));

        receiver.sendSystemMessage(Component.literal(
                "§6You denied §c" + sender.getDisplayName().getString() + "§6's §cTPA request!"
        ));
    }

    @SubscribeEvent
    public static void onTPACancelEvent(TeleportRequestCancelledEvent event) {
        ServerPlayer sender = event.getSender();
        ServerPlayer receiver = event.getReceiver();

        // Notify sender and receiver about cancellation
        sender.sendSystemMessage(Component.literal(
                "§6You cancelled your §cTPA request §6to §c" + sender.getDisplayName().getString()
        ));

        receiver.sendSystemMessage(Component.literal(
                "§c" + receiver.getDisplayName().getString() + " §6cancelled their §cTPA request!"
        ));
    }
}
