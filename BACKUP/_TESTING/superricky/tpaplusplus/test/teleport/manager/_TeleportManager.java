package _TESTING.superricky.tpaplusplus.test.teleport.manager;

import _TESTING.superricky.tpaplusplus.test.teleport._TeleportHere;
import _TESTING.superricky.tpaplusplus.test.teleport._TeleportTo;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import _TESTING.superricky.tpaplusplus.test._Config;
import _TESTING.superricky.tpaplusplus.test._Main;
import _TESTING.superricky.tpaplusplus.test.teleport._Teleport;
import org.jetbrains.annotations.NotNull;

import javax.annotation.Nullable;
import java.util.Objects;

public class _TeleportManager {
    private _TeleportManager() {}

    public static void sendTeleportTo(_TeleportTo teleportToRequest) throws IllegalArgumentException {
        /* The reason why we first add the @Nullable annotator, then catch it here, so we can add our own message instead of getting:
         * "An unexpected error occurred" in our output
         */
        if (Objects.isNull(teleportToRequest)) throw new IllegalArgumentException("Teleport-TO request is null!");
        ServerPlayer executor = teleportToRequest.executor();
        ServerPlayer teleported = teleportToRequest.teleported();

        // Protect against NullPointerException
        if (Objects.isNull(executor) || Objects.isNull(teleported)) throw new IllegalArgumentException("Received null ServerPlayer object(s)");

        // Notify and return if there already is a teleport request in the HashMap
        if (_TeleportHelper.alreadySentTeleportRequest(teleportToRequest)) {
            executor.sendSystemMessage(Component.literal("§cYou already sent a teleport request to this player!"));
            executor.sendSystemMessage(Component.literal("§6Run §c/tpacancel §6to cancel that request!"));
            return;
        }

        _Main.TELEPORT_REQUESTS.add(teleportToRequest);
        _TeleportScheduler.scheduleTeleportTimeout(
                teleportToRequest, _Config.TPA_TIMEOUT_IN_SECONDS.get());

        executor.sendSystemMessage(Component.literal("§6Sent teleport request to §c" + teleported.getDisplayName().getString()));
        teleported.sendSystemMessage(Component.literal("§6Teleport request received from §c" + executor.getDisplayName().getString()));
    }

    public static void sendTeleportHere(_TeleportHere teleportHereRequest) throws IllegalArgumentException {
        /* The reason why we first add the @Nullable annotator, then catch it here, so we can add our own message instead of getting:
         * "An unexpected error occurred" in our output
         */
        if (Objects.isNull(teleportHereRequest)) throw new IllegalArgumentException("Teleport-HERE request is null!");

        ServerPlayer executor = teleportHereRequest.executor();
        ServerPlayer teleported = teleportHereRequest.teleported();

        // Protect against NullPointerException
        if (Objects.isNull(executor) || Objects.isNull(teleported)) throw new IllegalArgumentException("Received null ServerPlayer object(s)");

        // Notify and return if there already is a teleport request in the HashMap
        if (_TeleportHelper.alreadySentTeleportRequest(teleportHereRequest)) {
            executor.sendSystemMessage(Component.literal("§cYou already sent a teleport §fhere §crequest to this player!"));
            executor.sendSystemMessage(Component.literal("§6Run §c/tpacancel §6to cancel that request!"));
            return;
        }

        _Main.TELEPORT_REQUESTS.add(teleportHereRequest);
        _TeleportScheduler.scheduleTeleportTimeout(
                teleportHereRequest, _Config.TPA_TIMEOUT_IN_SECONDS.get());

        executor.sendSystemMessage(Component.literal("§6Sent teleport §fhere §6request to §c" + teleported.getDisplayName().getString()));
        teleported.sendSystemMessage(Component.literal("§6Teleport §fhere §6request received from §c" + executor.getDisplayName().getString()));
    }

    /**
     * If a request is absolute, even if the tpa accept time in seconds is set to 0 ( disabled ), then we will still proceed with the request and instantly teleport the player over.
     * If a request is not absolute, if the tpa accept time in seconds is not disabled, then we will start a countdown to whatever it's configured to in the config,
     * once that countdown reaches 0 (code available in EventHandler.java), we will run this method with absolute turned on.
     */
    public static void acceptTeleportRequest(@Nullable _Teleport teleportRequest, boolean absolute) throws IllegalArgumentException {
        /* The reason why we first add the @Nullable annotator, then catch it here, so we can add our own message instead of getting:
         * "An unexpected error occurred" in our output
         */
        if (Objects.isNull(teleportRequest)) throw new IllegalArgumentException("Teleport request is null!");

        if (absolute || _Config.TPA_ACCEPT_TIME_IN_SECONDS.get() == 0) { // accept the TPA request if the request was absolute, or if the TPA accept timeout was disabled in the config
            absoluteAccept(teleportRequest);
        } else {
            teleportRequest.teleported().sendSystemMessage(Component.literal("§6You are being teleported..."));
            _Main.TELEPORTING_PLAYERS.add(teleportRequest);
            _TeleportScheduler.startTPAAcceptCountdown(
                    teleportRequest, _Config.TPA_ACCEPT_TIME_IN_SECONDS.get());
        }
    }

    // No need for @Nullable or catching the teleportRequest problem, since that is already handled in the acceptTeleportRequest class!
    private static void absoluteAccept(@NotNull _Teleport teleportRequest) throws IllegalArgumentException {
        ServerPlayer executor = teleportRequest.executor();
        ServerPlayer teleported = teleportRequest.teleported();

        // Protect against NullPointerException
        if (Objects.isNull(executor) || Objects.isNull(teleported))
            throw new IllegalArgumentException("Received null ServerPlayer object(s)");

        if (teleportRequest instanceof _TeleportTo) {
            _TeleportHelper.teleportPlayerTo(executor, teleported);

            executor.sendSystemMessage(Component.literal("§6Your teleport §6request for §c" + teleported.getDisplayName().getString() + " §6was accepted!"));
            teleported.sendSystemMessage(Component.literal("§6Accepted teleport §6request from §c" + executor.getDisplayName().getString()));
        } else if (teleportRequest instanceof _TeleportHere) {
            _TeleportHelper.teleportPlayerHere(executor, teleported);

            executor.sendSystemMessage(Component.literal("§6Your teleport §fhere §6request for §c" + teleported.getDisplayName().getString() + " §6was accepted!"));
            teleported.sendSystemMessage(Component.literal("§6Accepted teleport §fhere §6request from §c" + executor.getDisplayName().getString()));
        }

        _Main.TELEPORT_REQUESTS.remove(teleportRequest);
    }

    public static void denyTeleportRequest(@Nullable _Teleport teleportRequest) throws IllegalArgumentException {
        /* The reason why we first add the @Nullable annotator, then catch it here, so we can add our own message instead of getting:
         * "An unexpected error occurred" in our output
         */
        if (Objects.isNull(teleportRequest)) throw new IllegalArgumentException("Teleport request is null!");

        ServerPlayer executor = teleportRequest.executor();
        ServerPlayer teleported = teleportRequest.teleported();

        // Protect against NullPointerException
        if (Objects.isNull(executor) || Objects.isNull(teleported)) throw new IllegalArgumentException("Received null ServerPlayer object(s)");

        if (teleportRequest instanceof _TeleportTo) {
            executor.sendSystemMessage(Component.literal("§6Your teleport §6request for §c" + teleported.getDisplayName().getString() + " §6was denied!"));
            teleported.sendSystemMessage(Component.literal("§6Denied teleport §6request from §c" + executor.getDisplayName().getString()));
        } else if (teleportRequest instanceof _TeleportHere) {
            executor.sendSystemMessage(Component.literal("§6Your teleport §fhere §6request for §c" + teleported.getDisplayName().getString() + " §6was denied!"));
            teleported.sendSystemMessage(Component.literal("§6Denied teleport §fhere §6request from §c" + executor.getDisplayName().getString()));
        }
        _Main.TELEPORTING_PLAYERS.remove(teleportRequest);
        _Main.TELEPORT_REQUESTS.remove(teleportRequest);
    }

    public static void cancelTeleportRequest(@Nullable _Teleport teleportRequest) throws IllegalArgumentException {
        /* The reason why we first add the @Nullable annotator, then catch it here, so we can add our own message instead of getting:
         * "An unexpected error occurred" in our output
         */
        if (Objects.isNull(teleportRequest)) throw new IllegalArgumentException("Teleport request is null!");

        ServerPlayer executor = teleportRequest.executor();
        ServerPlayer teleported = teleportRequest.teleported();

        // Protect against NullPointerException
        if (Objects.isNull(executor) || Objects.isNull(teleported))
            throw new IllegalArgumentException("Received null ServerPlayer object(s)");

        if (teleportRequest instanceof _TeleportTo) {
            executor.sendSystemMessage(Component.literal("§6Denied teleport §6request for §c" + teleported.getDisplayName().getString()));
            teleported.sendSystemMessage(Component.literal("§6Your teleport §6request from §c" + executor.getDisplayName().getString() + " §6was cancelled!"));
        } else if (teleportRequest instanceof _TeleportHere) {
            executor.sendSystemMessage(Component.literal("§6Cancelled teleport §fhere §6request for §c" + teleported.getDisplayName().getString()));
            teleported.sendSystemMessage(Component.literal("§6Your teleport §fhere §6request from §c" + executor.getDisplayName().getString() + " §6was cancelled!"));
        }
        _Main.TELEPORTING_PLAYERS.remove(teleportRequest);
        _Main.TELEPORT_REQUESTS.remove(teleportRequest);
    }
}
