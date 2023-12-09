package net.superricky.tpaplusplus;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.living.LivingDeathEvent;
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
    /**
     * Our custom event. This event is triggered once the timer of a teleport request reaches 0, all we do here, is notify all members which were affected.
     * @param event
     */
    @SubscribeEvent
    public static void onTimeoutEvent(TeleportRequestTimeoutEvent event) {
        Teleport teleportRequest = event.getTeleportRequest();

        ServerPlayer executor = teleportRequest.executor();
        ServerPlayer teleported = teleportRequest.teleported();

        if (teleportRequest instanceof TeleportTo) {
            executor.sendSystemMessage(Component.literal("§6Your teleport request to §c" + teleported.getDisplayName().getString() + "§6 timed out!"));
            teleported.sendSystemMessage(Component.literal("§6Your teleport request from §c" + executor.getDisplayName().getString() + "§6 timed out!"));
        } else if (teleportRequest instanceof TeleportHere) {
            executor.sendSystemMessage(Component.literal("§6Your teleport §fhere §6request to §c" + teleported.getDisplayName().getString() + "§6 timed out!"));
            teleported.sendSystemMessage(Component.literal("§6Your teleport §fhere §6request from §c" + executor.getDisplayName().getString() + "§6 timed out!"));
        }

    }

    /**
     * Triggered on every tick.
     * Every tick, we decrease the teleportRequests timeout by 1.
     * This is why it is multiplied by 20 in our Main class.
     * Once this counter reaches 0, we remove the entry from the list and post a TimeoutEvent, which will notify all players affected that their TPA request expired.
     * @param event
     */
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

    /**
     * Triggered when an entity dies. Here we check if the entity is a player, if so then we log its death position, and remove the old one.
     * If it is not, then we skip it.
     * The reason why we still run this code even when it is disabled, is so if a player disables or enables "/back" during runtime, it will actually still keep this logged.
     * I might add this as a config option later on, if you are that concerned about performance.
     * @param event
     */
    @SubscribeEvent
    public static void onDeath(LivingDeathEvent event) {
        LivingEntity deadEntity = event.getEntity();
        ServerPlayer playerEntity;

        if (deadEntity instanceof ServerPlayer) {
            playerEntity = (ServerPlayer) event.getEntity();
        } else return;

        Vec3 deathPosition = playerEntity.position();

        // Remove old playerDeathCoordinate if present.
        Main.playerDeathCoordinates.remove(playerEntity);

        // Add this playerDeathCoordinate to the map.
        Main.playerDeathCoordinates.put(playerEntity, deathPosition);
    }
}
