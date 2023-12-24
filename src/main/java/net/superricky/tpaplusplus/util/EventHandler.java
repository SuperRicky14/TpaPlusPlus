package net.superricky.tpaplusplus.util;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.event.entity.living.LivingDeathEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.superricky.tpaplusplus.Messages;
import net.superricky.tpaplusplus.event.RequestAcceptSuccessEvent;
import net.superricky.tpaplusplus.event.RequestTimeoutEvent;
import org.apache.commons.lang3.NotImplementedException;

@Mod.EventBusSubscriber
public class EventHandler {
    /**
     * Our custom event. This event is triggered once the timer of a teleport request reaches 0, notifying all members which were affected.
     * @param event
     */
    @SubscribeEvent
    public static void onTimeoutEvent(RequestTimeoutEvent event) {
        Request request = event.getRequest();

        // Check if the request has not been accepted or denied, so you don't print timeout messages twice.
        if (RequestManager.requestSet.contains(request)) return;

        ServerPlayer receiver = request.getReceiver();
        ServerPlayer sender = request.getSender();

        if (request.isHereRequest()) {
            sender.sendSystemMessage(Component.literal(String.format(Messages.SENDER_TPAHERE_TIMEOUT.get(), receiver.getDisplayName().getString())));
            receiver.sendSystemMessage(Component.literal(String.format(Messages.RECEIVER_TPAHERE_TIMEOUT.get(), sender.getDisplayName().getString())));
            RequestManager.requestSet.remove(request);
            return;
        }

        sender.sendSystemMessage(Component.literal(String.format(Messages.SENDER_TPA_TIMEOUT.get(), receiver.getDisplayName().getString())));
        receiver.sendSystemMessage(Component.literal(String.format(Messages.RECEIVER_TPA_TIMEOUT.get(), sender.getDisplayName().getString())));
        RequestManager.requestSet.remove(request);
    }

    /*
    public static void decrementTPAAcceptTime(TickEvent.ServerTickEvent event) {
        if (Config.TPA_ACCEPT_TIME_IN_SECONDS.get() == 0) return; // return if it is disabled in the config

        for (Request request : RequestManager.requestSet) {
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
                    Main.teleportingPlayers.remove(teleportRequest);
                }
            }
        }

     */

    /**
     * Triggered when a TPAAcceptTimer is successful.
     * Here we just run our acceptTeleportRequest method from TeleportManager.java with ABSOLUTE mode enabled, so the player will be teleported to the other player.
     * This is what will happen after the 5 4 3 2 1 ( or whatever countdown in the config ) is finished!
     * @param event
     */
    @SubscribeEvent
    public static void onTPAAcceptTimerSuccess(RequestAcceptSuccessEvent event) {
        throw new NotImplementedException("Accept functionality is not yet implemented!");
        //RequestManager.acceptTeleportRequest(event.getTeleportRequest(), true);
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
        DeathPos deathPosition = new DeathPos(playerEntity.serverLevel(), playerEntity.position().toVector3f());

        // Remove old playerDeathCoordinate if present.
        DeathManager.playerDeathCoordinates.remove(playerEntity);

        // Add this playerDeathCoordinate to the map.
        DeathManager.playerDeathCoordinates.put(playerEntity, deathPosition);
    }
}
