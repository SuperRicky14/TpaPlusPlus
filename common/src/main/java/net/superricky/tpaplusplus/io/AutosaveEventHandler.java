package net.superricky.tpaplusplus.io;

public class AutosaveEventHandler {
    public static void onServerStart() {
        SaveDataManager.loadPlayerData(); // load data that was saved before
        AutosaveScheduler.initialiseAutoSaveService(); // start auto-saving
    }

    public static void onServerStop() {
        SaveDataManager.savePlayerData();
    }

    private AutosaveEventHandler() {
    }
}
