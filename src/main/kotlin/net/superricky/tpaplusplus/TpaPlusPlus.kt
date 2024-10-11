package net.superricky.tpaplusplus

import dev.architectury.event.events.common.CommandRegistrationEvent
import dev.architectury.event.events.common.LifecycleEvent
import dev.architectury.platform.Platform
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import net.fabricmc.api.ModInitializer
import net.minecraft.server.MinecraftServer
import net.superricky.tpaplusplus.GlobalConst.CONFIG_FILE_NAME
import net.superricky.tpaplusplus.GlobalConst.CONFIG_FILE_PATH
import net.superricky.tpaplusplus.GlobalConst.CONFIG_FOLDER_PATH
import net.superricky.tpaplusplus.GlobalConst.EXAMPLE_LANG_FILE_PATH
import net.superricky.tpaplusplus.GlobalConst.LANG_FOLDER_PATH
import net.superricky.tpaplusplus.GlobalConst.MOD_ID
import net.superricky.tpaplusplus.GlobalConst.logger
import net.superricky.tpaplusplus.command.CommandRegister
import net.superricky.tpaplusplus.config.config.CommonSpec
import net.superricky.tpaplusplus.config.config.Config
import net.superricky.tpaplusplus.config.config.Config.get
import net.superricky.tpaplusplus.config.language.LanguageConfig
import net.superricky.tpaplusplus.database.service.DataService
import net.superricky.tpaplusplus.event.ServerEvent
import java.nio.file.Files
import kotlin.coroutines.CoroutineContext

object TpaPlusPlus : ModInitializer, CoroutineScope {
    lateinit var server: MinecraftServer
    override val coroutineContext: CoroutineContext = Dispatchers.IO
    val version: String = Platform.getMod(MOD_ID).version.toString()
    lateinit var dataService: DataService

    override fun onInitialize() {
        init()
    }

    fun init() {
        logger.info("Initializing TPA++ $version")

        logger.info("Checking config...")
        if (!Files.isDirectory(Platform.getConfigFolder().resolve(CONFIG_FOLDER_PATH))) {
            logger.info("Config folder not exist, Creating.")
            Files.createDirectories(Platform.getConfigFolder().resolve(CONFIG_FOLDER_PATH))
        }

        if (!Files.exists(Platform.getConfigFolder().resolve(CONFIG_FILE_PATH))) {
            logger.info("No config file, Creating")
            Files.copy(
                Platform.getMod(MOD_ID).findResource(CONFIG_FILE_NAME).get(),
                Platform.getConfigFolder().resolve(CONFIG_FILE_PATH)
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

        logger.info("Checking lang file...")
        if (!Files.exists(Platform.getConfigFolder().resolve(LANG_FOLDER_PATH))) {
            logger.info("Lang folder not exist, Creating")
            Files.createDirectories(Platform.getConfigFolder().resolve(LANG_FOLDER_PATH))
        }

        if (!Files.exists(
                Platform.getConfigFolder().resolve(CONFIG_FOLDER_PATH).resolve(EXAMPLE_LANG_FILE_PATH)
            )
        ) {
            logger.info("No example lang file, Creating")
            Files.copy(
                Platform.getMod(MOD_ID).findResource(EXAMPLE_LANG_FILE_PATH).get(),
                Platform.getConfigFolder().resolve(CONFIG_FOLDER_PATH).resolve(EXAMPLE_LANG_FILE_PATH)
            )
        }

        logger.info("Loading lang file...")
        try {
            LanguageConfig.loadLangFile(CommonSpec.language.get().lowercase())
            logger.info("Language file loaded.")
        } catch (e: Exception) {
            logger.error("Error while loading lang file", e)
            return
        }

        logger.info("Register commands...")
        CommandRegistrationEvent.EVENT.register { dispatcher, _, _ ->
            CommandRegister.registerCommands(
                dispatcher
            )
        }

        logger.info("Add lifecycle event listener")
        LifecycleEvent.SERVER_BEFORE_START.register(ServerEvent::serverStartingEvent)
        LifecycleEvent.SERVER_STOPPED.register(ServerEvent::serverStoppedEvent)
        logger.info("Basic component initialization is complete")
    }
}
