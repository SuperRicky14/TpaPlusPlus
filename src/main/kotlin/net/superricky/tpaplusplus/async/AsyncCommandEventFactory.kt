package net.superricky.tpaplusplus.async

class AsyncCommandEventFactory {
    private val listeners: MutableMap<AsyncCommandEvent, MutableSet<Function1<AsyncCommandData, Unit>>> = mutableMapOf()

    fun addListener(event: AsyncCommandEvent, listener: Function1<AsyncCommandData, Unit>): AsyncCommandEventFactory {
        if (!listeners.containsKey(event)) {
            listeners[event] = HashSet()
        }
        listeners[event]!!.add(listener)
        return this
    }

    fun invoke(event: AsyncCommandEvent, asyncCommandData: AsyncCommandData): AsyncCommandEventFactory {
        if (!listeners.containsKey(event)) {
            listeners[event] = HashSet()
            return this
        }
        for (listener in listeners[event]!!) {
            listener.invoke(asyncCommandData)
        }
        return this
    }

    companion object {
        fun addListener(
            event: AsyncCommandEvent,
            listener: Function1<AsyncCommandData, Unit>
        ): AsyncCommandEventFactory {
            return AsyncCommandEventFactory().addListener(event, listener)
        }
    }
}
