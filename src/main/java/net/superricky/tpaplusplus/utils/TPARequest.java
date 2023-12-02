package net.superricky.tpaplusplus.utils;

import net.minecraft.server.level.ServerPlayer;

public class TPARequest {
    public TPARequest(ServerPlayer sender, ServerPlayer receiver, long timestamp, RequestType type) {
        this.sender = sender;
        this.receiver = receiver;
        this.timestamp = timestamp;
        this.type = type;
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

    public RequestType getType() {
        return type;
    }

    ServerPlayer sender;
    ServerPlayer receiver;
    long timestamp;

    /**
     * Two types:
     * "teleport_to": this is to teleport to the person you sent the tpa request to, via /tpa
     * "teleport_here": this is to teleport the person you sent the request to, to your position via /tpahere
     * */
    RequestType type;
}
