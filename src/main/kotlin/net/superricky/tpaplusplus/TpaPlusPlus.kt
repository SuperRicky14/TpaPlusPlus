package net.superricky.tpaplusplus

import dev.architectury.event.events.common.CommandRegistrationEvent
import dev.architectury.event.events.common.EntityEvent
import dev.architectury.event.events.common.PlayerEvent
import dev.architectury.event.events.common.TickEvent
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.cancel
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
import net.superricky.tpaplusplus.async.AsyncCommandHelper
import net.superricky.tpaplusplus.command.CommandRegister
import net.superricky.tpaplusplus.config.AdvancedSpec
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.database.service.DataService
import net.superricky.tpaplusplus.database.DataManager
import java.nio.file.Files
import kotlin.coroutines.CoroutineContext
import net.superricky.tpaplusplus.utility.PlayerEvent as PlayerEventListener

object TpaPlusPlus : ModInitializer, CoroutineScope {
    lateinit var server: MinecraftServer
    override val coroutineContext: CoroutineContext = Dispatchers.IO
    val version: Version = FabricLoader.getInstance().getModContainer(MOD_ID).get().metadata.version
    lateinit var dataService: DataService

    override fun onInitialize() {
        logger.info("Initializing TPA++ ${version.friendlyString}")

        logger.info("Checking config...")
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

        logger.info("Register commands...")
        CommandRegistrationEvent.EVENT.register { dispatcher, _, _ ->
            CommandRegister.registerCommands(
                dispatcher
            )
        }

        logger.info("Add lifecycle event listener")
        ServerLifecycleEvents.SERVER_STARTING.register(::serverStarting)
        ServerLifecycleEvents.SERVER_STOPPED.register(::serverStopped)
        logger.info("Basic component initialization is complete")
    }

    private fun serverStarting(server: MinecraftServer) {
        logger.info("Server starting...")
        logger.info("Main logic initializing...")
        this.server = server
        this.dataService = DataManager

        logger.info("Data service initializing...")
        dataService.initDataService()

        logger.info("Add player event listener...")
        PlayerEvent.PLAYER_JOIN.register(PlayerEventListener::joinEvent)
        EntityEvent.LIVING_DEATH.register(PlayerEventListener::deathEvent)

        if (Config.getConfig()[AdvancedSpec.unblockingTickLoop]) {
            logger.info("Using non blocking tick loop")
            logger.info(
                "Initializing non blocking tick loop with rate of " +
                        "${Config.getConfig()[AdvancedSpec.asyncLoopRate]} per second"
            )
            AsyncCommandHelper.startTickLoop(Config.getConfig()[AdvancedSpec.asyncLoopRate])
        } else {
            logger.info("Using synchronous tick loop")
            logger.info("Registering server post tick event")
            TickEvent.SERVER_POST.register { AsyncCommandHelper.runTick() }
        }
    }

    private fun serverStopped(ignored: MinecraftServer) {
        logger.info("Shutting down TPA++")
        AsyncCommandHelper.stopTickLoop()
        coroutineContext.cancel()
        dataService.saveData()
    }
}
