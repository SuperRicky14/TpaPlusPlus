package net.superricky.tpaplusplus.requests

import net.minecraft.server.level.ServerPlayer
import net.superricky.tpaplusplus.requests.RequestHelper.isPlayerIdentical

/**
 * A utility class used inside the request manager for grabbing teleport requests from the requestSet, based on the players point of view.
 * This is important as since we have commands that the receiver runs, that also have to grab the same teleport request that was sent by the sender,
 * and vice-versa, meaning that we have to use something like this since there is no one-size-fits-all solution here.
 */
object RequestGrabUtil {
    fun getSenderRequest(sender: ServerPlayer): Request? {
        for (request in RequestHelper.requestSet) {
            if (request.sender.isPlayerIdentical(sender)) {
                return request
            }
        }
        return null
    }

    fun getSenderRequest(sender: ServerPlayer, receiver: ServerPlayer): Request? {
        for (request in RequestHelper.requestSet) {
            if (request.sender.isPlayerIdentical(sender) &&
                request.receiver.isPlayerIdentical(receiver)
            ) {
                return request
            }
        }
        return null
    }

    fun getReceiverRequest(receiver: ServerPlayer): Request? {
        for (request in RequestHelper.requestSet) {
            if (request.receiver.isPlayerIdentical(receiver)) {
                return request
            }
        }
        return null
    }

    fun getReceiverRequest(receiver: ServerPlayer, sender: ServerPlayer): Request? {
        for (request in RequestHelper.requestSet) {
            if (request.receiver.isPlayerIdentical(receiver) &&
                request.sender.isPlayerIdentical(sender)
            ) {
                return request
            }
        }
        return null
    }
}
