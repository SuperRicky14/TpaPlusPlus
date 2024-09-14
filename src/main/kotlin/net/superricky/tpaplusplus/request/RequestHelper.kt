package net.superricky.tpaplusplus.request

import net.minecraft.server.network.ServerPlayerEntity

object RequestHelper {
    private val requestSet: MutableSet<Request> = HashSet()

    fun getRequestSet(): MutableSet<Request> = requestSet

    fun clearRequestSet() = requestSet.clear()

    fun teleportRequestExists(requestToFind: Request): Boolean {
        requestSet.forEach {
            if (requestToFind.sender == it.sender && requestToFind.sender == it.receiver) {
                return true
            }
        }
        return false
    }

    fun alreadySentTeleportRequest(sender: ServerPlayerEntity, receiver: ServerPlayerEntity): Boolean {
        requestSet.forEach {
            if (sender == it.sender && receiver == it.receiver) {
                return true
            }
        }
        return false
    }

    fun teleport(request: Request) {
        val sender = request.sender
        val receiver = request.receiver

        if (request.hereRequest) {
            receiver.teleport(sender.serverWorld, sender.x, sender.y, sender.z, sender.yaw, sender.pitch)
        }

        sender.teleport(receiver.serverWorld, receiver.x, receiver.y, receiver.z, receiver.yaw, receiver.pitch)
    }

    fun getSenderRequest(sender: ServerPlayerEntity): Request? =
        requestSet.firstOrNull {
            it.sender == sender
        }

    fun getSenderRequest(sender: ServerPlayerEntity, receiver: ServerPlayerEntity): Request? =
        requestSet.firstOrNull {
            it.sender == sender && it.receiver == receiver
        }

    fun getReceiverRequest(receiver: ServerPlayerEntity): Request? =
        requestSet.firstOrNull {
            it.receiver == receiver
        }

    fun getReceiverRequest(receiver: ServerPlayerEntity, sender: ServerPlayerEntity): Request? =
        requestSet.firstOrNull {
            it.sender == sender && it.receiver == receiver
        }
}
