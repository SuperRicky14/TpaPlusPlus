package net.superricky.tpaplusplus.database

import com.google.gson.GsonBuilder
import com.google.gson.reflect.TypeToken
import kotlinx.coroutines.delay
import kotlinx.coroutines.isActive
import kotlinx.coroutines.launch
import net.minecraft.server.network.ServerPlayerEntity
import net.superricky.tpaplusplus.GlobalConst.ONE_SECOND
import net.superricky.tpaplusplus.GlobalConst.PLAYER_DATA_FILE_NAME
import net.superricky.tpaplusplus.GlobalConst.logger
import net.superricky.tpaplusplus.TpaPlusPlus
import net.superricky.tpaplusplus.config.AdvancedSpec
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.utility.LevelBoundVec3
import java.io.File
import java.io.FileReader
import java.io.FileWriter
import java.io.IOException
import java.nio.file.Files
import java.util.*

object PlayerDataManager : DataService {
    private var playerDataMap: MutableMap<UUID, PlayerData> = Collections.synchronizedMap(HashMap())
    private val gson = GsonBuilder().setPrettyPrinting().create()
    private val dataSavePath: File = Config.getDatabasePath().resolve(PLAYER_DATA_FILE_NAME).toFile()

    init {
        if (!dataSavePath.exists()) {
            logger.info("Data file not exist, creating")
            Files.createFile(dataSavePath.toPath())
            FileWriter(dataSavePath).use {
                it.write("{}")
            }
        }
    }

    override fun initDataService() {
        loadPlayerData()
        TpaPlusPlus.launch {
            while (isActive) {
                savePlayerData()
                delay(Config.getConfig()[AdvancedSpec.autoSaveInterval] * ONE_SECOND)
            }
        }
    }

    override fun getPlayerData(player: ServerPlayerEntity): PlayerData {
        require(playerDataMap.containsKey(player.uuid)) { "PlayerData does not exist." }
        return playerDataMap[player.uuid]!!
    }

    override fun addBlockPlayer(uuid: UUID, blockPlayer: UUID): Boolean {
        if (playerDataMap.containsKey(uuid) && playerDataMap[uuid] != null) {
            playerDataMap[uuid]!!.blockPlayers.add(blockPlayer)
            return true
        }
        return false
    }

    override fun removeBlockPlayer(uuid: UUID, blockPlayer: UUID): Boolean {
        if (playerDataMap.containsKey(uuid) && playerDataMap[uuid] != null) {
            playerDataMap[uuid]!!.blockPlayers.remove(blockPlayer)
            return true
        }
        return false
    }

    override fun playerSwitchBlock(uuid: UUID): Boolean {
        if (playerDataMap.containsKey(uuid) && playerDataMap[uuid] != null) {
            playerDataMap[uuid]!!.toggle = !playerDataMap[uuid]!!.toggle
            return playerDataMap[uuid]!!.toggle
        }
        return false
    }

    override fun playerSwitchBlock(uuid: UUID, toggle: Boolean): Boolean {
        if (playerDataMap.containsKey(uuid) && playerDataMap[uuid] != null) {
            playerDataMap[uuid]!!.toggle = toggle
            return true
        }
        return false
    }

    override fun insertPlayer(uuid: UUID) {
        if (!playerDataMap.containsKey(uuid) || playerDataMap[uuid] == null) {
            playerDataMap[uuid] = PlayerData()
        }
    }

    override fun insertDeath(uuid: UUID, pos: LevelBoundVec3) {
        val playerData = playerDataMap[uuid]!!
        playerData.lastDeathPos.x = pos.x
        playerData.lastDeathPos.y = pos.y
        playerData.lastDeathPos.z = pos.z
        playerData.lastDeathPos.world = pos.serverLevel.value.toString()
        playerData.lastDeathPos.backed = false
    }

    override fun savePlayerData() {
        try {
            FileWriter(dataSavePath).use {
                gson.toJson(playerDataMap, it)
            }
        } catch (e: IOException) {
            logger.error("An IOException occurred when trying to save playerData.")
            logger.error(e.message)
        }
    }

    private fun loadPlayerData() {
        try {
            FileReader(dataSavePath).use {
                playerDataMap = gson.fromJson<Map<UUID, PlayerData>>(
                    it,
                    object : TypeToken<Map<UUID, PlayerData>>() {}.type
                ).toMutableMap()
                logger.info("Successfully loaded player data!")
            }
        } catch (e: IOException) {
            logger.error("An IOException occurred when trying to load playerData.")
            logger.error(e.message)
        }
    }
}
