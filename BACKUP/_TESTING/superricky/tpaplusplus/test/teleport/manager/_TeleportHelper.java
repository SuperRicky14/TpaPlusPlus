package _TESTING.superricky.tpaplusplus.test.teleport.manager;

import net.minecraft.server.level.ServerPlayer;
import _TESTING.superricky.tpaplusplus.test._Main;
import _TESTING.superricky.tpaplusplus.test.teleport._Teleport;

import javax.annotation.Nullable;

public class _TeleportHelper {
    private _TeleportHelper() {}

    @Nullable
    public static _Teleport getTeleportRequestByExecutorTeleported(ServerPlayer executor, ServerPlayer teleported) {
        for (_Teleport teleportRequest : _Main.TELEPORT_REQUESTS) {
            if (teleportRequest.executor().equals(executor)
                    && teleportRequest.teleported().equals(teleported))
                    return teleportRequest; }

        return null; // No matching teleport request
    }

    @Nullable
    public static _Teleport getTeleportRequestByTeleportedExecutor(ServerPlayer teleported, ServerPlayer executor) {
        for (_Teleport teleportRequest : _Main.TELEPORT_REQUESTS) {
            if (teleportRequest.teleported().equals(teleported)
                    && teleportRequest.executor().equals(executor))
                return teleportRequest; }

        return null; // No matching teleport request
    }

    @Nullable
    public static _Teleport getTeleportRequestByExecutor(ServerPlayer executor) {
        for (_Teleport teleportRequest : _Main.TELEPORT_REQUESTS) {
            if (teleportRequest.executor().equals(executor))
                    return teleportRequest; }
        return null; // No matching teleport request
    }

    @Nullable
    public static _Teleport getTeleportRequestByTeleported(ServerPlayer teleported) {
        for (_Teleport teleportRequest : _Main.TELEPORT_REQUESTS) {
            if (teleportRequest.teleported().equals(teleported))
                return teleportRequest; }
        return null; // No matching teleport request
    }

    static void teleportPlayerTo(ServerPlayer executor, ServerPlayer teleported) {
        executor.teleportTo(teleported.serverLevel(), teleported.getX(), teleported.getY(), teleported.getZ(), teleported.getYRot(), teleported.getXRot());
    }

    static void teleportPlayerHere(ServerPlayer executor, ServerPlayer teleported) {
        teleported.teleportTo(executor.serverLevel(), executor.getX(), executor.getY(), executor.getZ(), executor.getYRot(), executor.getXRot());
    }

    static boolean alreadySentTeleportRequest(_Teleport teleportRequest) {
        for (_Teleport currentTeleportRequest : _Main.TELEPORT_REQUESTS) {
            if (currentTeleportRequest.executor().equals(teleportRequest.executor())
                    && currentTeleportRequest.teleported().equals(teleportRequest.teleported()))
                            return true; }
        return false; // No matching teleport request
    }
}
