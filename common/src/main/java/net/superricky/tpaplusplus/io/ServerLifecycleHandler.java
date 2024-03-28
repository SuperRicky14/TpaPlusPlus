package net.superricky.tpaplusplus.io;

import net.superricky.tpaplusplus.TPAPlusPlus;
import net.superricky.tpaplusplus.timeout.TimeoutScheduler;
import net.superricky.tpaplusplus.windupcooldown.windup.AsyncWindup;
import net.superricky.tpaplusplus.windupcooldown.windup.WindupWatcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ServerLifecycleHandler {
    private static final Logger LOGGER = LoggerFactory.getLogger(TPAPlusPlus.MOD_ID);

    public static void onServerStart() {
        SaveDataManager.loadPlayerData(); // load data that was saved before
        AutosaveScheduler.initialiseAutoSaveService(); // start auto-saving
    }

    public static void onServerStop() {
        SaveDataManager.savePlayerData();

        try {
            LOGGER.info("Shutting down ScheduledExecutorService for AsyncWindup...");
            AsyncWindup.forceQuitScheduledExecutorService();
        } catch (InterruptedException e) {
            printServerStopError(e);
        }

        try {
            LOGGER.info("Shutting down ScheduledExecutorService for WindupWatcher...");
            WindupWatcher.forceQuitScheduledExecutorService();
        } catch (InterruptedException e) {
            printServerStopError(e);
        }

        try {
            LOGGER.info("Shutting down ScheduledExecutorService for TimeoutScheduler...");
            TimeoutScheduler.forceQuitScheduledExecutorService();
        } catch (InterruptedException e) {
            printServerStopError(e);
        }

        try {
            LOGGER.info("Shutting down ScheduledExecutorService for AutosaveScheduler...");
            AutosaveScheduler.forceQuitScheduledExecutorService();
        } catch (InterruptedException e) {
            printServerStopError(e);
        }
    }

    private static void printServerStopError(InterruptedException e) {
        LOGGER.error("InterruptedException: Failed to force quit ScheduledExecutorService. This will be skipped, if your server does not automatically stop, it should be safe to kill it AFTER minecraft has finished saving (After \"ThreadedAnvilChunkStorage: All dimensions are saved\").");
        LOGGER.error("Please report this to TPA++'s issue page immediately.");
        LOGGER.error(e.getMessage());
    }

    private ServerLifecycleHandler() {
    }
}
