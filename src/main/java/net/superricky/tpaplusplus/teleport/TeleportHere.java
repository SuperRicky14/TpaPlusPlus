package net.superricky.tpaplusplus.teleport;

import net.minecraft.server.level.ServerPlayer;

public record TeleportHere(ServerPlayer executor, ServerPlayer teleported) implements Teleport { }