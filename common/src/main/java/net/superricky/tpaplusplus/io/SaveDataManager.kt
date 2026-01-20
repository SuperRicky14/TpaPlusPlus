@file:UseSerializers(UUIDSerializer::class)
package net.superricky.tpaplusplus.io

import com.google.gson.JsonSyntaxException
import com.mojang.logging.LogUtils
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.UseSerializers
import kotlinx.serialization.builtins.MapSerializer
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.decodeFromStream
import kotlinx.serialization.json.encodeToStream
import net.minecraft.server.level.ServerPlayer
import org.slf4j.Logger
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
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

    @OptIn(ExperimentalSerializationApi::class)
    fun savePlayerData() {
        if (!MOD_SAVEDATA_FOLDER.exists()) {
            val success = MOD_SAVEDATA_FOLDER.mkdirs()
            if (!success) {
                LOGGER.error("Failed to automatically create TPAPlusPlus's savedata folder, consider creating $MOD_SAVEDATA_FILE_PATH manually!")
                return
            }
        }

        try {
            synchronized (saveDataLock) {
                FileOutputStream(MOD_SAVEDATA_FILE_PATH).use { writer ->
                    // We have to manually pass in our serializer here since Kotlinx.serialization's @Serializable annotation doesn't work for field types. See https://github.com/Kotlin/kotlinx.serialization/issues/2731 for more info
                    Json.encodeToStream(MapSerializer(UUIDSerializer, PlayerData.serializer()), saveData, writer)
                }
            }
        } catch (e: IOException) {
            LOGGER.error("An IOException occurred when trying to save playerData.")
            LOGGER.error(e.message)
        }
    }

    @OptIn(ExperimentalSerializationApi::class)
    fun loadPlayerData() = synchronized (saveDataLock) {
        if (!MOD_SAVEDATA_FOLDER.exists()) return

        try {
            FileInputStream(MOD_SAVEDATA_FILE_PATH).use { reader ->
                // We have to manually pass in our serializer here since Kotlinx.serialization's @Serializable annotation doesn't work for field types. See https://github.com/Kotlin/kotlinx.serialization/issues/2731 for more info
                saveData = Json.decodeFromStream(MapSerializer(UUIDSerializer, PlayerData.serializer()), reader).toMutableMap()
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
