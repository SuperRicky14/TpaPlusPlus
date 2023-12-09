package net.superricky.tpaplusplus.teleport;

import net.minecraft.server.level.ServerPlayer;

public interface Teleport {
    ServerPlayer executor();
    ServerPlayer teleported();
}
