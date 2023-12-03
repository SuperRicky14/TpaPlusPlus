package net.superricky.tpaplusplus;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.superricky.tpaplusplus.event.TeleportRequestTimeoutEvent;
import net.superricky.tpaplusplus.teleport.Teleport;
import net.superricky.tpaplusplus.teleport.TeleportHere;
import net.superricky.tpaplusplus.teleport.TeleportTo;

import java.util.Iterator;
import java.util.Map;

@Mod.EventBusSubscriber
public class EventHandler {
    @SubscribeEvent
    public static void onTimeoutEvent(TeleportRequestTimeoutEvent event) {
        Teleport teleportRequest = event.getTeleportRequest();

        ServerPlayer executor = teleportRequest.getExecutor();
        ServerPlayer teleported = teleportRequest.getTeleported();

        if (teleportRequest instanceof TeleportTo) {
            executor.sendSystemMessage(Component.literal("§6Your teleport request to §c" + teleported.getDisplayName().getString() + "§6 timed out!"));
            teleported.sendSystemMessage(Component.literal("§6Your teleport request from §c" + executor.getDisplayName().getString() + "§6 timed out!"));
        } else if (teleportRequest instanceof TeleportHere) {
            executor.sendSystemMessage(Component.literal("§6Your teleport §fhere §6request to §c" + teleported.getDisplayName().getString() + "§6 timed out!"));
            teleported.sendSystemMessage(Component.literal("§6Your teleport §fhere §6request from §c" + executor.getDisplayName().getString() + "§6 timed out!"));
        }

    }

    @SubscribeEvent
    public static void onServerTick(TickEvent.ServerTickEvent event) {
        Iterator<Map.Entry<Teleport, Integer>> iterator = Main.teleportRequests.entrySet().iterator();

        while (iterator.hasNext()) {
            Map.Entry<Teleport, Integer> entry = iterator.next();
            Teleport teleport = entry.getKey();
            int remainingTime = entry.getValue();

            if (remainingTime > 0) {
                // Decrement the remaining time by 1
                entry.setValue(remainingTime - 1);

                if (remainingTime - 1 == 0) {
                    MinecraftForge.EVENT_BUS.post(new TeleportRequestTimeoutEvent(teleport));
                    // When the time reaches 0, remove the entry
                    iterator.remove(); // Removes the current entry from the iterator
                    // Optionally, perform any additional actions when the time reaches 0
                    // For example: teleportRequests.remove(teleport);
                }
            }
        }
    }
}
