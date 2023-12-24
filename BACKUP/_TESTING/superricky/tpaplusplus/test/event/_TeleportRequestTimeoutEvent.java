package _TESTING.superricky.tpaplusplus.test.event;

import net.minecraftforge.eventbus.api.Cancelable;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.fml.common.Mod;
import _TESTING.superricky.tpaplusplus.test.teleport._Teleport;

@Cancelable
@Mod.EventBusSubscriber
public class _TeleportRequestTimeoutEvent extends Event {
    private final _Teleport teleportRequest;

    public _TeleportRequestTimeoutEvent(_Teleport teleportRequest) {
        this.teleportRequest = teleportRequest;
    }


    public _Teleport getTeleportRequest() {
        return teleportRequest;
    }
}
