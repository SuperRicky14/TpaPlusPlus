package _TESTING.superricky.tpaplusplus.test.teleport;

import net.minecraft.server.level.ServerPlayer;

public record _TeleportHere(ServerPlayer executor, ServerPlayer teleported) implements _Teleport { }