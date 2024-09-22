package net.superricky.tpaplusplus.database

import net.minecraft.server.network.ServerPlayerEntity
import net.superricky.tpaplusplus.utility.LevelBoundVec3
import java.util.*

interface DataService {
    fun initDataService()
    fun getPlayerData(player: ServerPlayerEntity): PlayerData
    fun addBlockPlayer(uuid: UUID, blockPlayer: UUID): Boolean
    fun removeBlockPlayer(uuid: UUID, blockPlayer: UUID): Boolean
    fun playerSwitchBlock(uuid: UUID): Boolean
    fun playerSwitchBlock(uuid: UUID, toggle: Boolean): Boolean
    fun insertPlayer(uuid: UUID)
    fun insertDeath(uuid: UUID, pos: LevelBoundVec3)
    fun savePlayerData()
}
