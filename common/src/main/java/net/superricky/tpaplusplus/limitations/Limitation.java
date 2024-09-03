package net.superricky.tpaplusplus.limitations;

import net.minecraft.server.level.ServerPlayer;

public interface Limitation {
    boolean isViolated(ServerPlayer sender, ServerPlayer receiver);
    String getViolationMessage(ServerPlayer sender, ServerPlayer receiver);
}
