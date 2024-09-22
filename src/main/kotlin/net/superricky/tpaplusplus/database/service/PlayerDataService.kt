package net.superricky.tpaplusplus.database.service

import net.minecraft.server.network.ServerPlayerEntity
import net.superricky.tpaplusplus.database.PlayerData
import net.superricky.tpaplusplus.utility.LevelBoundVec3
import java.util.*

interface PlayerDataService {
    fun getPlayerData(player: ServerPlayerEntity): PlayerData
    fun insertPlayer(uuid: UUID)
    fun insertDeath(uuid: UUID, pos: LevelBoundVec3)
}
