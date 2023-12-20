package net.superricky.tpaplusplus.teleport;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.Config;
import net.superricky.tpaplusplus.Main;
import net.superricky.tpaplusplus.TeleportScheduler;

import java.util.Objects;

public record TeleportHere(ServerPlayer executor, ServerPlayer teleported) implements Teleport {
    @Override
    public void send() {
        // Protect against NullPointerException
        if (Objects.isNull(executor) || Objects.isNull(teleported)) throw new IllegalArgumentException("ServerPlayer object(s) is null!");

        // Notify and return if there already is a teleport request in the HashMap
        if (Teleport.alreadySentTeleportRequest(this)) {
            executor.sendSystemMessage(Component.literal("§cYou already sent a teleport §fhere §crequest to this player!"));
            executor.sendSystemMessage(Component.literal("§6Run §c/tpacancel §6to cancel that request!"));
            return;
        }

        Main.teleportRequests.put(this, Config.TPA_TIMEOUT_IN_SECONDS.get() * 20);
        TeleportScheduler.scheduleTeleportTimeout(this, Config.TPA_TIMEOUT_IN_SECONDS.get());

        executor.sendSystemMessage(Component.literal("§6Sent teleport §fhere §6request to §c" + teleported.getDisplayName().getString()));
        teleported.sendSystemMessage(Component.literal("§6Teleport §fhere §6request received from §c" + executor.getDisplayName().getString()));
    }

    @Override
    public void accept(boolean absolute) {
        // Protect against NullPointerException
        if (Objects.isNull(executor) || Objects.isNull(teleported)) throw new IllegalArgumentException("ServerPlayer object(s) is null!");

        if (absolute || Config.TPA_COUNTDOWN_IN_SECONDS.get() == 0) { // accept the TPA request if the request was absolute, or if the TPA accept timeout was disabled in the config
            absoluteAccept();
        } else {
            teleported.sendSystemMessage(Component.literal("§6You are being teleported..."));
            Main.playerTeleportTime.put(this, Config.TPA_COUNTDOWN_IN_SECONDS.get() * 20);
            TeleportScheduler.startTPAAcceptCountdown(this, Config.TPA_COUNTDOWN_IN_SECONDS.get());
        }
    }

    /**
     * If a request is absolute, even if the tpa accept time in seconds is set to 0 ( disabled ), then we will still proceed with the request and instantly teleport the player over.
     * If a request is not absolute, if the tpa accept time in seconds is not disabled, then we will start a countdown to whatever it's configured to in the config,
     * once that countdown reaches 0 (code available in EventHandler.java), we will run this method with absolute turned on.
     */
    @Override
    public void absoluteAccept() {
        // Protect against NullPointerException
        if (Objects.isNull(executor) || Objects.isNull(teleported)) throw new IllegalArgumentException("Received null ServerPlayer(s)!");

        teleport();

        executor.sendSystemMessage(Component.literal("§6Your teleport §6request for §c" + teleported.getDisplayName().getString() + " §6was accepted!"));
        teleported.sendSystemMessage(Component.literal("§6Accepted teleport §6request from §c" + executor.getDisplayName().getString()));

        Main.teleportRequests.remove(this);
    }

    @Override
    public void deny() {
        // Protect against NullPointerException
        if (Objects.isNull(executor) || Objects.isNull(teleported)) throw new IllegalArgumentException("ServerPlayer object(s) is null!");

        executor.sendSystemMessage(Component.literal("§6Your teleport §fhere §6request for §c" + teleported.getDisplayName().getString() + " §6was denied!"));
        teleported.sendSystemMessage(Component.literal("§6Denied teleport §fhere §6request from §c" + executor.getDisplayName().getString()));

        Main.playerTeleportTime.remove(this);
        Main.teleportRequests.remove(this);
    }

    @Override
    public void cancel() {
        // Protect against NullPointerException
        if (Objects.isNull(executor) || Objects.isNull(teleported)) throw new IllegalArgumentException("ServerPlayer object(s) is null!");

        executor.sendSystemMessage(Component.literal("§6Cancelled teleport §fhere §6request for §c" + teleported.getDisplayName().getString()));
        teleported.sendSystemMessage(Component.literal("§6Your teleport §fhere §6request from §c" + executor.getDisplayName().getString() + " §6was cancelled!"));

        Main.playerTeleportTime.remove(this);
        Main.teleportRequests.remove(this);
    }

    // Helper methods
    public void teleport() {
        teleported.teleportTo(executor.serverLevel(), executor.getX(), executor.getY(), executor.getZ(), executor.getYRot(), executor.getXRot());
    }
}