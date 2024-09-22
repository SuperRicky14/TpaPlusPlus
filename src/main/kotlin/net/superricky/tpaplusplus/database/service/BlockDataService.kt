package net.superricky.tpaplusplus.database.service

import java.util.*

interface BlockDataService {
    fun checkPlayerBlocked(srcUUID: UUID, destUuid: UUID): Boolean
    fun addBlockPlayer(uuid: UUID, blockPlayer: UUID): Boolean
    fun removeBlockPlayer(uuid: UUID, blockPlayer: UUID): Boolean
}
