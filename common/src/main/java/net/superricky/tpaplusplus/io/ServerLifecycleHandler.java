package net.superricky.tpaplusplus.io;

import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.timeout.TimeoutManagerKt;

public class ServerLifecycleHandler {
    public static void onServerStart() {
        AutosaveSchedulerKt.initialiseAutoSaveService(Config.AUTOSAVE_INTERVAL_SECONDS.get());
    }

    public static void onServerStop() {
        AutosaveSchedulerKt.shutdownNow();
        TimeoutManagerKt.shutdownNow();

        SaveDataManager.savePlayerData();
    }

    private ServerLifecycleHandler() {
    }
}
