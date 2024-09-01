package net.superricky.tpaplusplus.network

import com.google.gson.Gson
import kotlinx.coroutines.*
import net.minecraft.network.chat.Component
import net.minecraft.server.level.ServerPlayer
import net.superricky.tpaplusplus.TPAPlusPlus
import net.superricky.tpaplusplus.config.Config
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.net.HttpURLConnection
import java.net.URL


private val LOGGER: Logger = LoggerFactory.getLogger(TPAPlusPlus.MOD_ID)
private val dispatcher: CoroutineDispatcher = Dispatchers.IO
private val scope: CoroutineScope = CoroutineScope(dispatcher)

/**
 * A method that effectively acts as a daemon, which will run our "executeSilentVersionCheck" method on the set time schedule.
 */
@OptIn(DelicateCoroutinesApi::class)
fun initVersionCheckDaemon() = GlobalScope.launch {
    if (Config.CHECK_FOR_UPDATES_INTERVAL_MINUTES.get() != 0L) {
        while (true) {
            executeSilentVersionCheck()
            delay(Config.CHECK_FOR_UPDATES_INTERVAL_MINUTES.get() * 1000L * 60L) // How often to check for updates. Change the first value to configure how often ( in minutes ) that version checking should take.
        }
    }
}

/**
 * Called when the player executes TPA++ version check in game.
 */
fun executeVersionCheck(executor: ServerPlayer) = scope.launch {
    val versionResponse = getVersionAsync()

    if (versionResponse.second == false) {
        val versionData = parseJSON(versionResponse.first, TPAPlusPlus.MOD_VERSION)
        when {
            versionData.component1() == -1 -> {
                LOGGER.warn("You are running an unofficial / modified version of TPA++. Expect bugs!")
                LOGGER.warn("You are running TPA++ version " + TPAPlusPlus.MOD_VERSION + ". The latest official version is " + versionData.component2())
                LOGGER.warn("If you got this version officially from the CurseForge / Modrinth, please wait 24 hours to see if Modrinth's API catches up.")
                LOGGER.warn("If waiting 24 hours does not resolve the issue, please open an issue report at \"https://github.com/SuperRicky14/TpaPlusPlus/issues\"")
                executor.sendSystemMessage(Component.literal("§eWARNING§f: You are running an unofficial / modified version of TPA++. Expect bugs!"))
                executor.sendSystemMessage(Component.literal("§eWARNING§f: You are running TPA++ version " + TPAPlusPlus.MOD_VERSION + ". The latest official version is " + versionData.component2()))
                executor.sendSystemMessage(Component.literal("§eWARNING§f: If you got this version officially from the CurseForge / Modrinth, please wait 24 hours to see if Modrinth's API catches up."))
                executor.sendSystemMessage(Component.literal("§eWARNING§f: If waiting 24 hours does not resolve the issue, please open an issue report at \"https://github.com/SuperRicky14/TpaPlusPlus/issues\""))
            }

            versionData.component1() > 0 -> {
                LOGGER.warn("You are running an outdated version of TPA++, latest version: " + versionData.component2() + ", your version: " + TPAPlusPlus.MOD_VERSION + "!")
                LOGGER.warn("You are " + versionData.component1() + " version(s) out of date!")
                executor.sendSystemMessage(Component.literal("§eWARNING§f: You are running an outdated version of TPA++, latest version: " + versionData.component2() + ", your version: " + TPAPlusPlus.MOD_VERSION + "!"))
                executor.sendSystemMessage(Component.literal("§eWARNING§f: You are " + versionData.component1() + " version(s) out of date!"))
            }

            versionData.component1() == 0 -> {
                LOGGER.info("You are running the latest version of TPA++")
                executor.sendSystemMessage(Component.literal("§6You are running the latest version of TPA++"))
            }
        }
    } else {
        LOGGER.error("Failed to check for updates: ${versionResponse.first}")
        executor.sendSystemMessage(Component.literal("§cFailed to check for updates: ${versionResponse.first}"))
    }
}

/**
 * Called when the player executes TPAPlusPlus version check from console
 */
fun executeVersionCheckFromConsole() = scope.launch {
    val versionResponse = getVersionAsync()

    if (versionResponse.second == false) {
        val versionData = parseJSON(versionResponse.first, TPAPlusPlus.MOD_VERSION)
        when {
            versionData.component1() == -1 -> {
                LOGGER.warn("You are running an unofficial / modified version of TPA++. Expect bugs!")
                LOGGER.warn("You are running TPA++ version " + TPAPlusPlus.MOD_VERSION)
                LOGGER.warn("If you got this version officially from the CurseForge / Modrinth, please wait 24 hours to see if Modrinth's API catches up.")
                LOGGER.warn("If waiting 24 hours does not resolve the issue, please open an issue report at \"https://github.com/SuperRicky14/TpaPlusPlus/issues\"")
            }

            versionData.component1() > 0 -> {
                LOGGER.warn("You are running an outdated version of TPA++, latest version: " + versionData.component2() + ", your version: " + TPAPlusPlus.MOD_VERSION + "!")
                LOGGER.warn("You are " + versionData.component1() + " version(s) out of date!")
            }

            versionData.component1() == 0 -> {
                LOGGER.info("You are running the latest version of TPA++")
            }
        }
    } else {
        LOGGER.error("Failed to check for updates: ${versionResponse.first}")
    }
}

/**
 * Called on the update checking schedule
 */
fun executeSilentVersionCheck() = scope.launch {
    val versionResponse = getVersionAsync()

    if (versionResponse.second == false) {
        val versionData = parseJSON(versionResponse.first, TPAPlusPlus.MOD_VERSION)
        when {
            versionData.component1() == -1 -> {
                LOGGER.warn("You are running an unofficial / modified version of TPA++. Expect bugs!")
                LOGGER.warn("You are running TPA++ version " + TPAPlusPlus.MOD_VERSION)
                LOGGER.warn("If you got this version officially from the CurseForge / Modrinth, please wait 24 hours to see if Modrinth's API catches up.")
                LOGGER.warn("If waiting 24 hours does not resolve the issue, please open an issue report at \"https://github.com/SuperRicky14/TpaPlusPlus/issues\"")
            }

            versionData.component1() > 0 -> {
                LOGGER.warn("You are running an outdated version of TPA++, latest version: " + versionData.component2() + ", your version: " + TPAPlusPlus.MOD_VERSION + "!")
                LOGGER.warn("You are " + versionData.component1() + " version(s) out of date!")
            }
        }
    } else if (versionResponse.second == true) {
        LOGGER.error("Failed to check for updates: ${versionResponse.first}")
    }
}

/**
 * Coroutine to get the latest version of TPA++ non-blockingly.
 * @return Pair<String response/error, Boolean failed>
 */
suspend fun getVersionAsync(): Pair<String, Boolean> = coroutineScope {
    val url = URL("https://api.modrinth.com/v2/project/QuPbmyCQ/version")

    withContext(dispatcher) {
        val conn = url.openConnection() as HttpURLConnection
        try {
            conn.requestMethod = "GET"
            conn.connect()

            val responseCode = conn.responseCode
            if (responseCode == HttpURLConnection.HTTP_OK) {
                // Read the response body
                val inputStream = conn.inputStream
                val response = inputStream.reader().readText()
                return@withContext Pair(response, false)
            } else {
                return@withContext Pair("Error: Bad HTTP response code $responseCode", true)
            }
        } catch (e: Exception) {
            return@withContext Pair("Error: ${e.message}", true)
        } finally {
            conn.disconnect()
        }
    }

}

/**
 * Black magic to parse our request using only GSON.
 */
fun parseJSON(jsonResponse: String, currentVersion: String): Pair<Int, String> {
    // Parse the JSON response into a list of maps
    val gson = Gson()
    val versions: List<Map<String, String>> = gson.fromJson(jsonResponse, List::class.java) as List<Map<String, String>>

    // Find the index of the current version
    val versionNumbers = versions.map { it["version_number"] ?: "" }
    val currentVersionIndex = versionNumbers.indexOf(currentVersion)

    // Find the latest version number
    val latestVersion = versions.maxByOrNull { it["version_number"]?.split('-')?.getOrNull(0)?.toIntOrNull() ?: 0 }
        ?.get("version_number") ?: ""

    return Pair(currentVersionIndex, latestVersion)
}
