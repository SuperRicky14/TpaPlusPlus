package net.superricky.tpaplusplus.requests

import net.minecraft.server.level.ServerPlayer

class Request(val sender: ServerPlayer, val receiver: ServerPlayer, val isHereRequest: Boolean) {
    override fun toString(): String {
        return "Request{sender=$sender, receiver=$receiver, hereRequest=$isHereRequest}"
    }
}
