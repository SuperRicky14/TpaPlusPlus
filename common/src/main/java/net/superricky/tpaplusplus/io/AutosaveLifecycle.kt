package net.superricky.tpaplusplus.io

import kotlinx.coroutines.CoroutineDispatcher
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.cancel
import kotlinx.coroutines.cancelChildren
import kotlinx.coroutines.launch
import kotlinx.coroutines.time.delay
import net.minecraft.server.MinecraftServer
import net.superricky.tpaplusplus.config.Config
import java.time.Duration

object AutosaveLifecycle {
    private val dispatcher: CoroutineDispatcher = Dispatchers.IO
    private val scope: CoroutineScope = CoroutineScope(dispatcher)

    private var autosaving = true

    fun onServerStart(server: MinecraftServer) {
        autosaving = true
        initialiseAutoSaveService(Config.AUTOSAVE_INTERVAL_SECONDS.get().toLong())
    }

    fun onServerStop(server: MinecraftServer) {
        autosaving = false
        scope.coroutineContext.cancelChildren()

        SaveDataManager.savePlayerData()
    }

    fun initialiseAutoSaveService(autosaveIntervalSeconds: Long) {
        SaveDataManager.loadPlayerData()
        scope.launch {
            while (autosaving) {
                SaveDataManager.savePlayerData()
                delay(Duration.ofSeconds(autosaveIntervalSeconds))
            }
        }
    }
}
