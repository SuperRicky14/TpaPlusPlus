package net.superricky.tpaplusplus.util.manager.saved;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
import com.mojang.logging.LogUtils;
import net.minecraft.server.level.ServerPlayer;
import org.slf4j.Logger;

import java.io.*;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public class SaveDataManager {
    private static final String MOD_SAVEDATA_FOLDER_PATH = "mods" + File.separator + ".tpaplusplus" + File.separator;
    private static final String MOD_SAVEDATA_FILE_PATH = MOD_SAVEDATA_FOLDER_PATH + File.separator + "tpaplusplus_savedata.json";
    private static final File MOD_SAVEDATA_FOLDER = new File(MOD_SAVEDATA_FOLDER_PATH);
    private static final Logger LOGGER = LogUtils.getLogger();

    private static Map<UUID, PlayerData> playerDataMap = Collections.synchronizedMap(new HashMap<>());

    public static PlayerData getPlayerData(ServerPlayer player) {
        for (Map.Entry<UUID, PlayerData> entry : playerDataMap.entrySet()) {
            if (entry.getKey().equals(player.getUUID())) return entry.getValue(); // return the playerData
        }
        // Didn't find playerData
        PlayerData newPlayerData = new PlayerData(); // Create PlayerData since it doesn't exist
        playerDataMap.put(player.getUUID(), newPlayerData); // Put the reference to our new PlayerData into the playerDataMap

        return newPlayerData; // Return the newly made PlayerData
    }

    public static void savePlayerData() {
        Gson gson = new GsonBuilder().setPrettyPrinting().create();

        if (Boolean.FALSE.equals(checkForSaveDataExistence())) return; // Make sure the folder(s) exist and were automatically created successfully before writing to the file
        try (Writer writer = new FileWriter(MOD_SAVEDATA_FILE_PATH)) {
            gson.toJson(playerDataMap, writer);
            LOGGER.info("Successfully saved player data!");
        } catch (IOException e) {
            LOGGER.error("An IOException occurred when trying to save playerData.");
            e.printStackTrace();
        }
    }

    public static void loadPlayerData() {
        Gson gson = new GsonBuilder().setPrettyPrinting().create();

        if (Boolean.FALSE.equals(checkForSaveDataExistenceForLoading())) return; // Make sure the folder(s) exist and were automatically created successfully before reading from the file
        try (Reader reader = new FileReader(MOD_SAVEDATA_FILE_PATH)) {
            playerDataMap = gson.fromJson(reader, new TypeToken<Map<UUID, PlayerData>>() {}.getType());
            LOGGER.info("Successfully loaded player data!");
        } catch (IOException e) {
            LOGGER.error("An IOException occurred when trying to load playerData.");
            e.printStackTrace();
        }
    }

    static boolean checkForSaveDataExistenceForLoading() {
        return MOD_SAVEDATA_FOLDER.exists();
    }

    static boolean checkForSaveDataExistence() {
        if (!MOD_SAVEDATA_FOLDER.exists()) {
            // Folder doesn't exist, create it
            boolean success = MOD_SAVEDATA_FOLDER.mkdirs();

            if (!success) {
                String errorMsg = String.format("Failed to automatically create TPAPlusPlus's savedata folder, consider creating %s manually!", MOD_SAVEDATA_FILE_PATH);
                LOGGER.error(errorMsg);  // Use the pre-formatted message
            }
            return success;
        }
        return true;
    }

    public static void replacePlayerData(UUID playerUUID, PlayerData playerData) {
        playerDataMap.remove(playerUUID);

        playerDataMap.put(playerUUID, playerData);
    }

    private SaveDataManager() {
    }
}
