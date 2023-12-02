package net.superricky.tpaplusplus.utils;

import net.minecraft.server.level.ServerPlayer;

import java.util.Objects;

public class SenderReceiverPair {
    private final ServerPlayer sender;
    private final ServerPlayer receiver;

    public SenderReceiverPair(ServerPlayer sender, ServerPlayer receiver) {
        this.sender = sender;
        this.receiver = receiver;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        SenderReceiverPair that = (SenderReceiverPair) o;
        return Objects.equals(sender, that.sender) && Objects.equals(receiver, that.receiver);
    }

    @Override
    public int hashCode() {
        return Objects.hash(sender, receiver);
    }
}
