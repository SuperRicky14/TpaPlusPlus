package net.superricky.tpaplusplus.io;

import net.superricky.tpaplusplus.config.Config;

public class ServerLifecycleHandler {
    public static void onServerStart() {
        AutosaveSchedulerKt.initialiseAutoSaveService(Config.AUTOSAVE_INTERVAL_SECONDS.get());
    }

    public static void onServerStop() {
        SaveDataManager.savePlayerData();
    }

    private ServerLifecycleHandler() {
    }
}
