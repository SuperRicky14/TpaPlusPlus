package net.superricky.tpaplusplus.util.manager;

import dev.architectury.event.EventResult;
import net.minecraft.network.chat.Component;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.LivingEntity;
import net.superricky.tpaplusplus.TPAPlusPlus;
import net.superricky.tpaplusplus.event.RequestMovedEvent;
import net.superricky.tpaplusplus.util.LevelBoundVec3;
import net.superricky.tpaplusplus.util.Request;
import net.superricky.tpaplusplus.util.configuration.Config;
import net.superricky.tpaplusplus.util.configuration.Messages;
import net.superricky.tpaplusplus.util.configuration.formatters.MessageParser;
import net.superricky.tpaplusplus.util.manager.saved.SaveDataManager;

import java.util.Map;

public class EventHandler {
    private EventHandler() {}

    /**
     * Our custom event.
     * This event is triggered once the timer of a teleport request reaches 0, notifying all members that were affected.
     */
    public static EventResult onTimeoutEvent(Request request) {
        /* Check if the request has not been accepted or denied, so you don't print timeout messages multiple times.
         * UPDATE: This now ACTUALLY prevents printing timeout messages multiple times, because now the check is inverted (see RequestManager#alreadySentTeleportRequest),
         * since before we were only displaying your timeout message IF the timeout message did NOT expire, which you can imagine that caused problems.
         */
        if (Boolean.FALSE.equals(RequestManager.alreadySentTeleportRequest(request))) return EventResult.pass();

        ServerPlayer receiver = request.getReceiver();
        ServerPlayer sender = request.getSender();

        if (request.isHereRequest()) {
            sender.sendSystemMessage(Component.literal(String.format(Messages.SENDER_TPAHERE_TIMEOUT.get(), receiver.getDisplayName().getString())));
            receiver.sendSystemMessage(Component.literal(String.format(Messages.RECEIVER_TPAHERE_TIMEOUT.get(), sender.getDisplayName().getString())));
            RequestManager.requestSet.remove(request);
            return EventResult.pass();
        }

        sender.sendSystemMessage(Component.literal(String.format(Messages.SENDER_TPA_TIMEOUT.get(), receiver.getDisplayName().getString())));
        receiver.sendSystemMessage(Component.literal(String.format(Messages.RECEIVER_TPA_TIMEOUT.get(), sender.getDisplayName().getString())));
        RequestManager.requestSet.remove(request);
        return EventResult.pass();
    }

    /**
     * Triggered when a TPAAcceptTimer is successful.
     * Here we just run our acceptTeleportRequest method from TeleportManager.java with ABSOLUTE mode enabled, so the player will be teleported to the other player.
     * This is what will happen after the 5 4 3 2 1 (or whatever countdown in the config) is finished!
     */
    public static EventResult onTPAAcceptTimerSuccess(Request request) {
        RequestManager.acceptFunctionality(request, request.getReceiver(), true);
        return EventResult.pass();
    }

    public static EventResult onTPAMove(Request request) {
        if (request.isHereRequest()) {
            request.getReceiver().sendSystemMessage(Component.literal(Messages.RECEIVER_MOVED_DURING_COUNTDOWN.get()));

            if (Boolean.TRUE.equals(Config.SEND_COUNTDOWN_MOVEMENT_CANCEL_TO_BOTH_PLAYERS.get()))
                    request.getSender().sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.SENDER_REQUEST_TO_RECEIVER_MOVED_DURING_COUNTDOWN.get(),
                            Map.of("receiverName", request.getReceiver().getName().getString()))));
        } else {
            request.getSender().sendSystemMessage(Component.literal(Messages.RECEIVER_MOVED_DURING_COUNTDOWN.get()));

            if (Boolean.TRUE.equals(Config.SEND_COUNTDOWN_MOVEMENT_CANCEL_TO_BOTH_PLAYERS.get()))
                request.getReceiver().sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.SENDER_REQUEST_TO_RECEIVER_MOVED_DURING_COUNTDOWN.get(),
                        Map.of("receiverName", request.getSender().getName().getString()))));
        }

        RequestManager.requestSet.remove(request);
        return EventResult.pass();
    }

    public static void onServerStart() {
        SaveDataManager.loadPlayerData(); // load data that was saved before
        AsyncTaskManager.AsyncAutosave.initialiseAutoSaveService(); // start auto-saving
    }

    public static void onServerStop() {
        SaveDataManager.savePlayerData();
    }

    /**
     * Measures the distance between the initial accept position and the current position of the player.
     * If the distance is greater than the allowed distance set in the config, the RequestMovedEvent is posted.
     * This event is triggered on a server tick event.
     */
    public static void measurePlayerDistanceOverAccept(MinecraftServer server) {
        if (Config.ALLOWED_MOVEMENT_DURING_ACCEPT_COUNTDOWN.get() == 0) return;

        for (Request request : RequestManager.requestSet) {
            // teleport request has not been accepted
            if (Boolean.FALSE.equals(request.isAccepted())) continue;

            if (request.isHereRequest()) {
                double distance = TPAPlusPlus.euclideanDistanceCalculator3D(
                        request.getReceiver().getX(),
                        request.getReceiver().getY(),
                        request.getReceiver().getZ(),
                        request.getAcceptX(),
                        request.getAcceptY(),
                        request.getAcceptZ()
                );

                // Distance between the initial accept position, and the current position of the receiver is GREATER than the allowed distance set in the config.
                if (distance > Config.ALLOWED_MOVEMENT_DURING_ACCEPT_COUNTDOWN.get()) {
                    RequestMovedEvent.EVENT.invoker().onRequestMoved(request);
                }
                continue;
            }
            double distance = TPAPlusPlus.euclideanDistanceCalculator3D(
                    request.getSender().getX(),
                    request.getSender().getY(),
                    request.getSender().getZ(),
                    request.getAcceptX(),
                    request.getAcceptY(),
                    request.getAcceptZ()
            );

            // Distance between the initial accept position, and the current position of the receiver is GREATER than the allowed distance set in the config.
            if (distance > Config.ALLOWED_MOVEMENT_DURING_ACCEPT_COUNTDOWN.get()) {
                RequestMovedEvent.EVENT.invoker().onRequestMoved(request);
            }
        }
    }

    /**
     * Triggered when an entity dies.
     * Here we check if the entity is a player, if so, then we log its death position, and remove the old one.
     * If it is not, then we skip it.
     * The reason why we still run this code even when it is disabled,
     * is so if a player disables or enables "/back" during runtime,
     * it will actually still keep this logged.
     * I might add this as a config option later on if you are that concerned about performance.
     */
    public static EventResult onDeath(LivingEntity deadEntity, DamageSource source) {
        // Create an empty ServerPlayer variable
        ServerPlayer playerEntity;

        // Return if the dead entity in question is not a player
        if (deadEntity instanceof ServerPlayer) {
            playerEntity = (ServerPlayer) deadEntity;
        } else return EventResult.pass();

        // Get the death position as a net.minecraft.world.phys Vec3 object
        LevelBoundVec3 deathPosition = new LevelBoundVec3(playerEntity.serverLevel(), playerEntity.getX(), playerEntity.getY(), playerEntity.getZ());

        // Remove old playerDeathCoordinate if present.
        DeathManager.playerDeathCoordinates.remove(playerEntity);

        // Add this playerDeathCoordinate to the map.
        DeathManager.playerDeathCoordinates.put(playerEntity, deathPosition);

        return EventResult.pass();
    }
}
