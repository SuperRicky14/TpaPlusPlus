package net.superricky.tpaplusplus.requests;

import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.formatters.MessageParser;

import java.util.Map;

public class Request {
    private final ServerPlayer sender;
    private final ServerPlayer receiver;
    private final boolean hereRequest;

    public Request(ServerPlayer sender, ServerPlayer receiver, boolean hereRequest) {
        this.sender = sender;
        this.receiver = receiver;
        this.hereRequest = hereRequest;
    }

    public ServerPlayer getSender() {
        return sender;
    }

    public ServerPlayer getReceiver() {
        return receiver;
    }

    public boolean isHereRequest() {
        return hereRequest;
    }

    @Override
    public String toString() {
        return "Request{" +
                "sender=" + sender.getName() +
                ", receiver=" + receiver.getName() +
                ", hereRequest=" + hereRequest +
                '}';
    }
}
