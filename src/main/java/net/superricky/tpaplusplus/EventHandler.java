package net.superricky.tpaplusplus;

import java.util.Map;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.living.LivingDeathEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.superricky.tpaplusplus.event.TPAAcceptSuccessEvent;
import net.superricky.tpaplusplus.event.TeleportRequestTimeoutEvent;
import net.superricky.tpaplusplus.teleport.Teleport;
import net.superricky.tpaplusplus.teleport.TeleportHere;
import net.superricky.tpaplusplus.teleport.TeleportTo;

@Mod.EventBusSubscriber
public class EventHandler {
    private EventHandler() {}

    /**
     * Our custom event. This event is triggered once the timer of a teleport request reaches 0, notifying all members which were affected.
     * @param event
     */
    @SubscribeEvent
    public static void onTimeoutEvent(TeleportRequestTimeoutEvent event) {
        Teleport teleportRequest = event.getTeleportRequest();

        ServerPlayer executor = teleportRequest.executor();
        ServerPlayer teleported = teleportRequest.teleported();

        if (teleportRequest instanceof TeleportTo) {
            executor.sendSystemMessage(Component.literal(String.format("§6Your teleport request to §c%s§6 timed out!", teleported.getDisplayName().getString())));
            teleported.sendSystemMessage(Component.literal(String.format("§6Your teleport request from §c%s§6 timed out!", executor.getDisplayName().getString())));
        } else if (teleportRequest instanceof TeleportHere) {
            executor.sendSystemMessage(Component.literal(String.format("§6Your teleport §fhere §6request to §c%s§6 timed out!", teleported.getDisplayName().getString())));
            teleported.sendSystemMessage(Component.literal(String.format("§6Your teleport §fhere §6request from §c%s§6 timed out!", executor.getDisplayName().getString())));
        }
    }

    /**
     * Triggered on every tick.
     * Every tick, we decrease the teleportRequests timeout by 1.
     * This is why it is multiplied by 20 in our Main class.
     * Once this counter reaches 0, we remove the entry from the list and post a TimeoutEvent, which will notify all players affected that their TPA request expired.
     * @param event
     */
    //@SubscribeEvent
    public static void decrementTeleportRequests(TickEvent.ServerTickEvent event) {
        if (Config.TPA_TIMEOUT_IN_SECONDS.get() == 0) return; // return if it is disabled in the config

        // Loop through every entry in the teleportRequests hashmap
        for (Map.Entry<Teleport, Integer> entry : Main.teleportRequests.entrySet()) {
            // Get how much time is remaining in the hashmap ( in game ticks )
            int remainingTime = entry.getValue();

            // If the remaining time is more than 0
            if (remainingTime > 0) {
                // Decrement remaining time by 1
                entry.setValue(remainingTime - 1);

                // If the remaining time is 0
                if (remainingTime - 1 == 0) {
                    Teleport teleportRequest = entry.getKey();
                    MinecraftForge.EVENT_BUS.post(new TeleportRequestTimeoutEvent(teleportRequest));

                    // Remove the entry
                    Main.teleportRequests.remove(teleportRequest);
                }
            }
        }
    }

    /**
     * Triggered on every tick.
     * Every tick, we decrease the playerTeleportTime timeout by 1.
     * This is why it is multiplied by 20 in our Main class.
     * Once this counter reaches 0, we remove the entry from the list and do something that I haven't figured out yet
     * @param event
     */
    //@SubscribeEvent
    public static void decrementTPAAcceptTime(TickEvent.ServerTickEvent event) {
        if (Config.TPA_COUNTDOWN_IN_SECONDS.get() == 0) return; // return if it is disabled in the config

        for (Map.Entry<Teleport, Integer> entry : Main.playerTeleportTime.entrySet()) {
            // Get how much time is remaining in the hashmap ( in game ticks)
            int remainingTime = entry.getValue();

            // If the remaining time is more than 0
            if (remainingTime > 0) {
                // Decrement remaining time by 1
                entry.setValue(remainingTime - 1);

                // Check if the remaining time is a multiple of 20 and if the number ISN'T 0.
                if (remainingTime % 20 == 0) {
                    // Get the player being teleported from the teleport request
                    ServerPlayer teleported = entry.getKey().teleported();

                    // Send how many seconds are remaining to the player
                    teleported.sendSystemMessage(Component.literal("§6" + remainingTime / 20));

                    return;
                }
                if (remainingTime - 1 == 0) {
                    Teleport teleportRequest = entry.getKey();
                    MinecraftForge.EVENT_BUS.post(new TPAAcceptSuccessEvent(teleportRequest));

                    // Remove the entry from the playerTeleportTime hashmap and the teleportRequests hashmap to stop de-sync
                    Main.teleportRequests.remove(teleportRequest);
                    Main.playerTeleportTime.remove(teleportRequest);
                }
            }
        }
    }

    /**
     * Triggered when a TPAAcceptTimer is successful.
     * Here we just run our acceptTeleportRequest method from TeleportManager.java with ABSOLUTE mode enabled, so the player will be teleported to the other player.
     * This is what will happen after the 5 4 3 2 1 ( or whatever countdown in the config ) is finished!
     * @param event
     */
    @SubscribeEvent
    public static void onTPAAcceptTimerSuccess(TPAAcceptSuccessEvent event) {
        event.getTeleportRequest().accept(true);
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
        // Get the dead entity and create an empty ServerPlayer variable
        LivingEntity deadEntity = event.getEntity();
        ServerPlayer playerEntity;

        // Return if the dead entity in question is not a player
        if (deadEntity instanceof ServerPlayer) {
            playerEntity = (ServerPlayer) event.getEntity();
        } else return;

        // Get the death position as a net.minecraft.world.phys Vec3 object
        Vec3 deathPosition = playerEntity.position();

        // Remove old playerDeathCoordinate if present.
        Main.playerDeathCoordinates.remove(playerEntity);

        // Add this playerDeathCoordinate to the map.
        Main.playerDeathCoordinates.put(playerEntity, deathPosition);
    }
}
