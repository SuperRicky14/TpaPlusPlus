package net.superricky.tpaplusplus.event;

import net.minecraftforge.eventbus.api.Cancelable;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.fml.common.Mod;
import net.superricky.tpaplusplus.teleport.Teleport;

@Cancelable
@Mod.EventBusSubscriber
public class TeleportRequestTimeoutEvent extends Event {
    private final Teleport teleportRequest;

    public TeleportRequestTimeoutEvent(Teleport teleportRequest) {
        this.teleportRequest = teleportRequest;
    }


    public Teleport getTeleportRequest() {
        return teleportRequest;
    }
}
