package net.superricky.tpaplusplus.requests

import net.minecraft.server.level.ServerPlayer

object RequestHelper {
    val requestSet: MutableSet<Request> = hashSetOf()

    fun ServerPlayer.isPlayerIdentical(otherPlayer: ServerPlayer): Boolean {
        return this.getUUID() == otherPlayer.getUUID()
    }

    fun clearRequestSet() {
        requestSet.clear()
    }

    fun teleportRequestExists(requestToFind: Request): Boolean {
        for (request in requestSet) {
            if (requestToFind.sender.isPlayerIdentical(request.sender)
                && requestToFind.receiver.isPlayerIdentical(request.receiver)
            ) {
                return true
            }
        }

        return false
    }

    fun alreadySentTeleportRequest(sender: ServerPlayer, receiver: ServerPlayer): Boolean {
        for (request in requestSet) {
            if (sender.isPlayerIdentical(request.sender)
                && receiver.isPlayerIdentical(request.receiver)
            ) return true
        }
        return false
    }

    fun teleport(request: Request) {
        val sender = request.sender
        val receiver = request.receiver

        // /tpahere
        if (request.isHereRequest) {
            receiver.teleportTo(
                sender.serverLevel(),
                sender.x,
                sender.y,
                sender.z,
                sender.yRot,
                sender.xRot,
            )
        }

        // /tpa
        sender.teleportTo(
            receiver.serverLevel(),
            receiver.x,
            receiver.y,
            receiver.z,
            receiver.yRot,
            receiver.xRot,
        )
    }
}
