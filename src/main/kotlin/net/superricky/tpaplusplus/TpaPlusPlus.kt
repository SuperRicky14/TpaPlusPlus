package net.superricky.tpaplusplus

import dev.architectury.event.events.common.CommandRegistrationEvent
import dev.architectury.event.events.common.PlayerEvent
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import net.fabricmc.api.ModInitializer
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerLifecycleEvents
import net.fabricmc.loader.api.FabricLoader
import net.fabricmc.loader.api.Version
import net.minecraft.server.MinecraftServer
import net.superricky.tpaplusplus.GlobalConst.CONFIG_FILE_NAME
import net.superricky.tpaplusplus.GlobalConst.CONFIG_FILE_PATH
import net.superricky.tpaplusplus.GlobalConst.CONFIG_FOLDER_PATH
import net.superricky.tpaplusplus.GlobalConst.MOD_ID
import net.superricky.tpaplusplus.GlobalConst.logger
import net.superricky.tpaplusplus.command.CommandRegister
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.database.DatabaseManager
import net.superricky.tpaplusplus.event.PlayerEvent as PlayerEventListener
import java.nio.file.Files
import kotlin.coroutines.CoroutineContext

object TpaPlusPlus : ModInitializer, CoroutineScope {
    lateinit var server: MinecraftServer
    override val coroutineContext: CoroutineContext = Dispatchers.IO
    val version: Version = FabricLoader.getInstance().getModContainer(MOD_ID).get().metadata.version

    override fun onInitialize() {
        logger.info("Initializing TPA++ ${version.friendlyString}")

        if (!Files.isDirectory(FabricLoader.getInstance().configDir.resolve(CONFIG_FOLDER_PATH))) {
            logger.info("Config folder not exist, Creating.")
            Files.createDirectories(FabricLoader.getInstance().configDir.resolve(CONFIG_FOLDER_PATH))
        }

        if (!Files.exists(FabricLoader.getInstance().configDir.resolve(CONFIG_FILE_PATH))) {
            logger.info("No config file, Creating")
            Files.copy(
                FabricLoader.getInstance().getModContainer(MOD_ID).get().findPath(CONFIG_FILE_NAME).get(),
                FabricLoader.getInstance().configDir.resolve(CONFIG_FILE_PATH)
            )
        }
        logger.info("Loading config file...")
        try {
            Config.loadAndVerifyConfig()
            logger.info("Config file loaded.")
        } catch (e: Exception) {
            logger.error("Error while loading config file", e)
            return
        }
        CommandRegistrationEvent.EVENT.register { dispatcher, _, _ ->
            CommandRegister.registerCommands(
                dispatcher
            )
        }
        PlayerEvent.PLAYER_JOIN.register(PlayerEventListener::joinEvent)
        ServerLifecycleEvents.SERVER_STARTING.register(::serverStarting)
        ServerLifecycleEvents.SERVER_STOPPED.register(::serverStopped)
    }

    private fun serverStarting(server: MinecraftServer) {
        logger.info("Starting TPA++ server")
        this.server = server
        DatabaseManager.setup()
        DatabaseManager.ensureTables()

        TpaPlusPlus.launch {
            DatabaseManager.setupCache()
        }
    }

    private fun serverStopped(ignored: MinecraftServer) {
        logger.info("Shutting down TPA++")
    }
}
