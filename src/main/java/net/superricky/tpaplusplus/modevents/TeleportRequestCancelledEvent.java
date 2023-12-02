package net.superricky.tpaplusplus.modevents;

import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.eventbus.api.Event;

public class TeleportRequestCancelledEvent extends Event {
    private ServerPlayer sender;
    private ServerPlayer receiver;

    public TeleportRequestCancelledEvent(ServerPlayer sender, ServerPlayer receiver) {
        this.sender = sender;
        this.receiver = receiver;
    }


    public ServerPlayer getSender() {
        return sender;
    }

    public ServerPlayer getReceiver() {
        return receiver;
    }
}
