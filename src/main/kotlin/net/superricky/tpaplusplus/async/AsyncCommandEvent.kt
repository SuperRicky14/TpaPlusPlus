package net.superricky.tpaplusplus.async

enum class AsyncCommandEvent {
    REQUEST_AFTER_DELAY, // Command windup finish event, take effect
    REQUEST_UPDATE_MESSAGE, // windup message update event, show message to player
    REQUEST_CANCELED, // Teleport canceled
    REQUEST_OUT_DISTANCE, // Move too much, fail to execute command
    REQUEST_TIMEOUT, // Request out of time event
    REQUEST_UNDER_COOLDOWN, // command under cooldown event
    REQUEST_ACCEPTED, // teleport request accepted event
    REQUEST_NOT_FOUND, // can not find teleport request event
    TELEPORT_OUT_DISTANCE, // move too much when teleporting
    TELEPORT_UPDATE_MESSAGE, // teleport message update event, show message to player
    USELESS_VOID // Placeholders, useless return values
}
