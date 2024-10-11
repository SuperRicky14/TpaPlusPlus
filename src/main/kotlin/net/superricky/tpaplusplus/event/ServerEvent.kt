package net.superricky.tpaplusplus.event

import dev.architectury.event.events.common.EntityEvent
import dev.architectury.event.events.common.TickEvent
import kotlinx.coroutines.cancel
import net.minecraft.server.MinecraftServer
import net.minecraft.server.network.ServerPlayerEntity
import net.superricky.tpaplusplus.GlobalConst.PERMISSION_LEVEL
import net.superricky.tpaplusplus.GlobalConst.logger
import net.superricky.tpaplusplus.TpaPlusPlus
import net.superricky.tpaplusplus.TpaPlusPlus.coroutineContext
import net.superricky.tpaplusplus.TpaPlusPlus.dataService
import net.superricky.tpaplusplus.async.AsyncCommandHelper
import net.superricky.tpaplusplus.config.config.AdvancedSpec
import net.superricky.tpaplusplus.config.config.Config
import net.superricky.tpaplusplus.config.config.Config.get
import net.superricky.tpaplusplus.config.language.LanguageConfig
import net.superricky.tpaplusplus.config.language.LanguageConfig.getMutableText
import net.superricky.tpaplusplus.config.language.SystemSpec
import net.superricky.tpaplusplus.database.DataManager
import net.superricky.tpaplusplus.utility.TextColorPallet

object ServerEvent {
    fun serverStartingEvent(server: MinecraftServer) {
        fun getOps(server: MinecraftServer): List<ServerPlayerEntity> {
            val ops = mutableListOf<ServerPlayerEntity>()
            server.playerManager.playerList.filterTo(ops) {
                it.hasPermissionLevel(PERMISSION_LEVEL)
            }
            return ops.toList()
        }
        logger.info("Server starting...")
        logger.info("Main logic initializing...")
        TpaPlusPlus.server = server
        dataService = DataManager

        logger.info("Data service initializing...")
        dataService.initDataService()

        logger.info("Add player event listener...")
        dev.architectury.event.events.common.PlayerEvent.PLAYER_JOIN.register(PlayerEvent::joinEvent)
        EntityEvent.LIVING_DEATH.register(PlayerEvent::deathEvent)

        Config.addLoadListener {
            getOps(server).forEach {
                it.sendMessage(SystemSpec.configChange.getMutableText().setStyle(TextColorPallet.primary))
            }
        }

        LanguageConfig.addLoadListener {
            getOps(server).forEach {
                it.sendMessage(SystemSpec.languageChange.getMutableText().setStyle(TextColorPallet.primary))
            }
        }

        if (AdvancedSpec.unblockingTickLoop.get()) {
            logger.info("Using non blocking tick loop")
            logger.info(
                "Initializing non blocking tick loop with rate of " +
                        "${AdvancedSpec.asyncLoopRate.get()} per second"
            )
            AsyncCommandHelper.startTickLoop(AdvancedSpec.asyncLoopRate.get())
        } else {
            logger.info("Using synchronous tick loop")
            logger.info("Registering server post tick event")
            TickEvent.SERVER_POST.register { AsyncCommandHelper.runTick() }
        }
    }

    fun serverStoppedEvent(ignored: MinecraftServer) {
        logger.info("Shutting down TPA++")
        AsyncCommandHelper.stopTickLoop()
        coroutineContext.cancel()
        dataService.saveData()
    }
}
