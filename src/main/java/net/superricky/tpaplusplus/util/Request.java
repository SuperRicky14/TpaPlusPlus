package net.superricky.tpaplusplus.util;

import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.Config;

public class Request {
    private final ServerPlayer sender;
    private final ServerPlayer receiver;
    private final long timestamp;

    private final boolean hereRequest;
    private boolean accepted;

    public Request(ServerPlayer sender, ServerPlayer receiver, boolean hereRequest) {
        this.sender = sender;
        this.receiver = receiver;
        this.timestamp = System.currentTimeMillis();
        this.hereRequest = hereRequest;
        this.accepted = false;
    }

    public ServerPlayer getSender() {
        return sender;
    }

    public ServerPlayer getReceiver() {
        return receiver;
    }

    public long getTimestamp() {
        return timestamp;
    }

    public boolean isHereRequest() {
        return hereRequest;
    }

    public boolean isAccepted() {
        return accepted;
    }

    public void setAccepted(boolean accepted) {
        this.accepted = accepted;
    }

    public boolean isTimedOut() {
        return Config.TPA_TIMEOUT_IN_SECONDS.get() == 0 || System.currentTimeMillis() - timestamp > Config.TPA_TIMEOUT_IN_SECONDS.get() * 1000;
    }
}
