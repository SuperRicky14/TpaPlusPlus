package net.superricky.tpaplusplus.io

import com.google.gson.GsonBuilder
import com.google.gson.JsonSyntaxException
import com.google.gson.reflect.TypeToken
import com.mojang.logging.LogUtils
import net.minecraft.server.level.ServerPlayer
import org.slf4j.Logger
import java.io.File
import java.io.FileReader
import java.io.FileWriter
import java.io.IOException
import java.util.*

private val MOD_SAVEDATA_FILE_NAME = "tpaplusplus_savedata.json"
private val MOD_SAVEDATA_FOLDER_PATH = "mods" + File.separator + ".tpaplusplus" + File.separator
private val MOD_SAVEDATA_FILE_PATH = MOD_SAVEDATA_FOLDER_PATH + File.separator + MOD_SAVEDATA_FILE_NAME
private val MOD_SAVEDATA_FOLDER = File(MOD_SAVEDATA_FOLDER_PATH)

object SaveDataManager {
    private val LOGGER: Logger = LogUtils.getLogger()

    private val saveDataLock = Any() // We use a separate lock object because we reassign saveData

    private var saveData: MutableMap<UUID, PlayerData> = hashMapOf()

    fun getPlayerData(player: ServerPlayer): PlayerData = synchronized (saveDataLock) {
        return saveData.getOrPut(player.uuid) {
            PlayerData()
        }
    }

    fun savePlayerData() {
        val gson = GsonBuilder().setPrettyPrinting().create()

        if (!MOD_SAVEDATA_FOLDER.exists()) {
            val success = MOD_SAVEDATA_FOLDER.mkdirs()
            if (!success) {
                LOGGER.error("Failed to automatically create TPAPlusPlus's savedata folder, consider creating $MOD_SAVEDATA_FILE_PATH manually!")
                return
            }
        }

        try {
            synchronized (saveDataLock) {
                FileWriter(MOD_SAVEDATA_FILE_PATH).use { writer ->
                    gson.toJson(saveData, writer)
                }
            }
        } catch (e: IOException) {
            LOGGER.error("An IOException occurred when trying to save playerData.")
            LOGGER.error(e.message)
        }
    }

    fun loadPlayerData() = synchronized (saveDataLock) {
        val gson = GsonBuilder().setPrettyPrinting().create()

        if (!MOD_SAVEDATA_FOLDER.exists()) return

        try {
            FileReader(MOD_SAVEDATA_FILE_PATH).use { reader ->
                saveData = gson.fromJson(reader, object : TypeToken<MutableMap<UUID, PlayerData>>() {}.type)
                LOGGER.info("Successfully loaded player data!")
            }
        } catch (e: IOException) {
            LOGGER.error("An IOException occurred when trying to load playerData.")
            LOGGER.error(e.message)
        } catch (e: JsonSyntaxException) {
            LOGGER.error("An JsonSyntaxException occurred when trying to load playerData.")
            LOGGER.error("Did you manually edit \"$MOD_SAVEDATA_FILE_NAME\"? If so, you might want to check your syntax!")
            LOGGER.error(e.message)
        }
    }
}
