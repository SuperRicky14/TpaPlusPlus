package net.superricky.tpaplusplus.event;

import net.minecraftforge.eventbus.api.Cancelable;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.fml.common.Mod;
import net.superricky.tpaplusplus.util.Request;

@Cancelable
@Mod.EventBusSubscriber
public class RequestTimeoutEvent extends Event {

    private final Request request;

    public RequestTimeoutEvent(Request request) {
            this.request = request;
    }

    public Request getRequest() {
            return request;
        }
}
