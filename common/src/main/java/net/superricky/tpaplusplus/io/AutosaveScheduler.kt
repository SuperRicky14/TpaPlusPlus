package net.superricky.tpaplusplus.io

import kotlinx.coroutines.*

private val dispatcher: CoroutineDispatcher = Dispatchers.IO
private val scope: CoroutineScope = CoroutineScope(dispatcher)

private var autosaving = true

fun initialiseAutoSaveService(autosaveIntervalSeconds: Long) {
    SaveDataManager.loadPlayerData()
    scope.launch {
        while (autosaving) {
            SaveDataManager.savePlayerData()
            delay(autosaveIntervalSeconds * 1000L)
        }
    }
}