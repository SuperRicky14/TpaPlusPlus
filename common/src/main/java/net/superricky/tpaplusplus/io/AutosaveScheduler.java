package net.superricky.tpaplusplus.io;

import net.superricky.tpaplusplus.config.Config;

import java.util.Objects;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

public class AutosaveScheduler {
    private static ScheduledExecutorService scheduler = Executors.unconfigurableScheduledExecutorService(Executors.newScheduledThreadPool(1));

    public static void initialiseAutoSaveService() {
        scheduler.schedule(AutosaveScheduler::autoSave, Config.AUTOSAVE_INTERVAL.get(), TimeUnit.SECONDS);
    }

    // Use recursion to continuously AutoSave the PlayerData.
    private static void autoSave() {
        SaveDataManager.savePlayerData();

        scheduler.schedule(AutosaveScheduler::autoSave, Config.AUTOSAVE_INTERVAL.get(), TimeUnit.SECONDS);
    }

    public static boolean stopScheduledExecutorService() throws InterruptedException {
        if (scheduler.isShutdown())
            throw new IllegalStateException("Attempted to shutdown the ScheduledExecutorService but it was already shutdown beforehand!");

        // Shutdown the ScheduledExecutorService immediately
        scheduler.shutdownNow();

        // Forcefully shutdown the executor
        return scheduler.awaitTermination(5, TimeUnit.SECONDS);
    }

    public static void reCreateScheduledExecutorService() {
        if (Boolean.FALSE.equals(scheduler.isShutdown()))
            throw new IllegalStateException("Attempted to re-create ScheduledExecutorService but it was not shutdown beforehand!");

        // Create a new ScheduledExecutorService
        scheduler = Executors.unconfigurableScheduledExecutorService(Executors.newScheduledThreadPool(1));
        initialiseAutoSaveService(); // We have to re-initialise the autosave ScheduledExecutorService
    }

    public static boolean forceQuitScheduledExecutorService() throws InterruptedException {
        // Shutdown the ScheduledExecutorService immediately
        scheduler.shutdownNow();

        // Forcefully shutdown the executor
        return scheduler.awaitTermination(5, TimeUnit.SECONDS);
    }

    private AutosaveScheduler() {
    }
}
