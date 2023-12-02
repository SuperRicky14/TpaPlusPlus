package net.superricky.tpaplusplus.utils;

import net.minecraft.server.level.ServerPlayer;

public class TPARequest {
    public TPARequest(ServerPlayer sender, ServerPlayer receiver, long timestamp, String type) {
        this.sender = sender;
        this.receiver = receiver;
        this.timestamp = timestamp;
        this.type = type;
    }
    ServerPlayer sender;
    ServerPlayer receiver;
    long timestamp;

    /**
     * Two types:
     * "teleport-to": this is to teleport to the person you sent the tpa request to, via /tpa
     * "teleport-here": this is to teleport the person you sent the request to, to your position via /tpahere
     * */
    String type;
    boolean accepted;
}
