@file:UseSerializers(UUIDSerializer::class)
package net.superricky.tpaplusplus.io

import com.mojang.logging.LogUtils
import kotlinx.io.files.FileNotFoundException
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.SerializationException
import kotlinx.serialization.UseSerializers
import kotlinx.serialization.builtins.MapSerializer
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.decodeFromStream
import kotlinx.serialization.json.encodeToStream
import net.minecraft.server.level.ServerPlayer
import org.slf4j.Logger
import java.io.IOException
import java.io.InputStream
import java.io.OutputStream
import java.nio.file.FileAlreadyExistsException
import java.nio.file.Files
import java.nio.file.Paths
import java.util.*

private val MOD_SAVE_DATA_FILE_NAME = "tpaplusplus_savedata.json"
private val SAVE_DATA_PATH = Paths.get("mods", ".tpaplusplus", MOD_SAVE_DATA_FILE_NAME)

object SaveDataManager {
    private val LOGGER: Logger = LogUtils.getLogger()

    private val saveDataLock = Any() // We use a separate lock object because we reassign saveData

    private var saveData: MutableMap<UUID, PlayerData> = hashMapOf()

    fun getPlayerData(player: ServerPlayer): PlayerData = synchronized (saveDataLock) {
        return saveData.getOrPut(player.uuid) {
            PlayerData()
        }
    }

    //region <Saving>
    fun savePlayerData() {
        try {
            Files.createDirectories(SAVE_DATA_PATH.parent)
        } catch (e: FileAlreadyExistsException) {
            LOGGER.error("""
                Failed to create save data folder. A file exists with the same name as one of the required folders in: "${SAVE_DATA_PATH.parent}"
                
                Please rename or remove the conflicting file(s) so that TPA++ can save data.
                """.trimIndent(), e)
            return
        } catch (e: IOException) {
            LOGGER.error("""
                An I/O error occurred whilst trying to create TPA++'s save data folder.
                
                If this happens consistently, consider manually creating the parent folder(s) at "${SAVE_DATA_PATH.parent}"
            """.trimIndent(), e)
            return
        }

        val saveDataSnapshot = synchronized (saveDataLock) {
            saveData.toMap() // TODO: This is a shallow copy, it is not currently thread safe as PlayerData is mutable
        }

        val outputStream = try {
            Files.newOutputStream(SAVE_DATA_PATH)
        } catch (e: IOException) {
            LOGGER.error("""
                An I/O error occurred whilst trying to open the save data file at "$SAVE_DATA_PATH".
            """.trimIndent(), e)
            return
        }

        outputStream.use {
            encodeSaveData(saveDataSnapshot, it)
        }
    }

    @OptIn(ExperimentalSerializationApi::class)
    fun encodeSaveData(saveDataSnapshot: Map<UUID, PlayerData>, outputStream: OutputStream) {
        try {
            // We have to manually pass in our serializer here since Kotlinx.serialization's @Serializable annotation doesn't work for field types. See https://github.com/Kotlin/kotlinx.serialization/issues/2731 for more info
            Json.encodeToStream(MapSerializer(UUIDSerializer, PlayerData.serializer()), saveDataSnapshot, outputStream)
        } catch (e: IOException) {
            LOGGER.error("An I/O error occurred whilst trying to write to TPA++'s save data stream", e)
            return
        } catch (e: SerializationException) {
            LOGGER.error("""
                Failed to serialize TPA++'s save data to JSON!
            """.trimIndent(), e)
            return
        }
    }
    //endregion

    //region <Loading>
    fun loadPlayerData() {
        val inputStream = try {
            Files.newInputStream(SAVE_DATA_PATH)
        } catch (_: FileNotFoundException) {
            LOGGER.info("TPA++ save data does not exist, aborting loading procedure.")
            return
        } catch (e: IOException) {
            LOGGER.error("An I/O error occurred whilst trying to read TPA++'s save data!", e)
            return
        }

        val deserializedPlayerData = inputStream.use {
            deserializePlayerData(it)
        }
        if (deserializedPlayerData == null) return

        synchronized (saveDataLock) { saveData = deserializedPlayerData.toMutableMap() }

    }

    @OptIn(ExperimentalSerializationApi::class)
    private fun deserializePlayerData(reader: InputStream): Map<UUID, PlayerData>? {
        try {
            // We have to manually pass in our serializer here since Kotlinx.serialization's @Serializable annotation doesn't work for field types. See https://github.com/Kotlin/kotlinx.serialization/issues/2731 for more info
            return Json.decodeFromStream(MapSerializer(UUIDSerializer, PlayerData.serializer()), reader)
        } catch (e: SerializationException) {
            LOGGER.error("""
                Failed to deserialize TPA++'s save data.
                Did you manually edit \"$MOD_SAVE_DATA_FILE_NAME\"? If so, you might want to check your syntax!
            """.trimIndent(), e)
        } catch (e: IllegalArgumentException) {
            LOGGER.error("""
                TPA++'s save data is not compatible with the internal format.
                Is the save data from a different version?
            """.trimIndent(), e)
        }
        return null
    }
    //endregion
}
