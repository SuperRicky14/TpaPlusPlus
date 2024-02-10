package net.superricky.tpaplusplus.event;

import dev.architectury.event.Event;
import dev.architectury.event.EventFactory;
import dev.architectury.event.EventResult;
import net.superricky.tpaplusplus.util.Request;

public interface RequestTimeoutEvent {
    Event<RequestTimeoutEvent> EVENT = EventFactory.createEventResult();

    EventResult onRequestTimeout(Request request);
}
