package net.superricky.tpaplusplus.io;

import net.superricky.tpaplusplus.TPAPlusPlus;
import net.superricky.tpaplusplus.config.Config;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ServerLifecycleHandler {
    private static final Logger LOGGER = LoggerFactory.getLogger(TPAPlusPlus.MOD_ID);

    public static void onServerStart() {
        AutosaveSchedulerKt.initialiseAutoSaveService(Config.AUTOSAVE_INTERVAL_SECONDS.get());
    }

    public static void onServerStop() {
        SaveDataManager.savePlayerData();
    }

    private static void printServerStopError(InterruptedException e) {
        LOGGER.error("InterruptedException: Failed to force quit ScheduledExecutorService. This will be skipped, if your server does not automatically stop, it should be safe to kill it AFTER minecraft has finished saving (After \"ThreadedAnvilChunkStorage: All dimensions are saved\").");
        LOGGER.error("Please report this to TPA++'s issue page immediately.");
        LOGGER.error(e.getMessage());
    }

    private ServerLifecycleHandler() {
    }
}
