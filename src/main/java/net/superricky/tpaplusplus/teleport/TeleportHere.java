package net.superricky.tpaplusplus.teleport;

import net.minecraft.server.level.ServerPlayer;

public class TeleportHere implements Teleport {
    private final ServerPlayer executor;
    private final ServerPlayer teleported;

    public TeleportHere(ServerPlayer executor, ServerPlayer teleported) {
        this.executor = executor;
        this.teleported = teleported;
    }


    @Override
    public ServerPlayer getExecutor() {
        return executor;
    }

    @Override
    public ServerPlayer getTeleported() {
        return teleported;
    }
}
