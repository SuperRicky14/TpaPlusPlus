package net.superricky.tpaplusplus.util.limitations;

import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;

public record Limitation(LimitationType type, Double distance, ServerLevel executorLevel, ServerLevel otherPlayerLevel, ServerPlayer outOfBoundsPlayer) {

    // Range check limitations
    public Limitation(LimitationType type, Double distance) {
        this(type, distance, null, null, null);
    }

    // Dimension check limitations
    public Limitation(LimitationType type, ServerLevel senderLevel, ServerLevel receiverLevel) {
        this(type, null, senderLevel, receiverLevel, null);
    }
}
