package net.superricky.tpaplusplus.event;

import dev.architectury.event.Event;
import dev.architectury.event.EventFactory;
import dev.architectury.event.EventResult;
import net.superricky.tpaplusplus.util.Request;

public interface RequestMovedEvent {
    Event<RequestMovedEvent> EVENT = EventFactory.createEventResult();

    EventResult onRequestMoved(Request request);
}
