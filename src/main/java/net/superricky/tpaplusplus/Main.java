package net.superricky.tpaplusplus;

import com.mojang.logging.LogUtils;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.config.ModConfig;
import net.superricky.tpaplusplus.teleport.Teleport;
import org.slf4j.Logger;

import java.io.*;
import java.util.HashMap;

// The value here should match an entry in the META-INF/mods.toml file
@Mod(Main.MOD_ID)
public class Main {
    // Our mod id
    public static final String MOD_ID = "tpaplusplus";

    // The teleport request is our Teleport part, the time remaining (in ticks) is the Integer part.
    public static HashMap<Teleport, Integer> teleportRequests = new HashMap<>();

    // Store every player's death coordinates in a hashmap.
    public static HashMap<ServerPlayer, Vec3> playerDeathCoordinates = new HashMap<>();
    private static final Logger LOGGER = LogUtils.getLogger();
    private static final String TPAPlusPlusSaveDataFile = "tpaplusplus_savedata.ser";

    public Main() {
        MinecraftForge.EVENT_BUS.register(this);

        // Instantiate the mods configuration
        ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, Config.SPEC, "tpaplusplus-common.toml");
    }
    /*
    public static void saveTeleportData() {
        // Do nothing if both saving systems are disabled
        LOGGER.info("Checking Configuration before loading data");
        if (!Config.SAVE_TELEPORT_REQUESTS_TO_DISK.get() && !Config.SAVE_DEATH_COORDS_TO_DISK.get()) return;
        LOGGER.info("Attempting to save data!");
        // Save the data to a string (TPAPlusPlusSaveDataFile) defined at the top of this file.
        try (FileOutputStream fileOut = new FileOutputStream(TPAPlusPlusSaveDataFile);
             ObjectOutputStream objectOut = new ObjectOutputStream(fileOut)) {

            // Only save player death coordinates to disk if the death coordinate saving system is enabled.
            if (Config.SAVE_DEATH_COORDS_TO_DISK.get()) {
                objectOut.writeObject("TELEPORT_REQUESTS");
                objectOut.writeObject(Main.playerDeathCoordinates);
            }

            // Only save teleport requests to disk if the death coordinate saving system is enabled.
            if (Config.SAVE_TELEPORT_REQUESTS_TO_DISK.get()) {
                objectOut.writeObject("PLAYER_DEATH_COORDINATES");
                objectOut.writeObject(Main.teleportRequests);
            }

            LOGGER.info("Data was successfully saved to \"" + TPAPlusPlusSaveDataFile + "\"");
        } catch (IOException e) { // Catch any IO exception and report back to console
            LOGGER.info(e.toString());
        }
    }

    @SuppressWarnings("unchecked")
    public static void loadTeleportData() {
        // Do nothing if both saving systems are disabled
        LOGGER.info("Checking Configuration before loading data");
        if (!Config.SAVE_TELEPORT_REQUESTS_TO_DISK.get() && !Config.SAVE_DEATH_COORDS_TO_DISK.get()) return;
        LOGGER.info("Attempting to load data!");
        // Save the data to a string (TPAPlusPlusSaveDataFile) defined at the top of this file.
        try (FileInputStream fileIn = new FileInputStream(TPAPlusPlusSaveDataFile);
             ObjectInputStream objectIn = new ObjectInputStream(fileIn)) {

            // Create an empty object
            Object obj;

            // Loop through the list, and assign each one to its own variable
            while ((obj = objectIn.readObject()) != null) {
                if (obj instanceof String identifier) {

                    // Only save player death coordinates to disk if the death coordinate saving system is enabled.
                    if (Config.SAVE_DEATH_COORDS_TO_DISK.get() && identifier.equals("PLAYER_DEATH_COORDINATES")) {
                        playerDeathCoordinates = (HashMap<ServerPlayer, Vec3>) objectIn.readObject();
                    }

                    // Only save teleport requests to disk if the death coordinate saving system is enabled.
                    else if (Config.SAVE_TELEPORT_REQUESTS_TO_DISK.get() && identifier.equals("TELEPORT_REQUESTS")) {
                        teleportRequests = (HashMap<Teleport, Integer>) objectIn.readObject();
                    }
                }
            }

            LOGGER.info("Data was successfully loaded from \"" + TPAPlusPlusSaveDataFile + "\"");
        } catch (IOException | ClassNotFoundException e) { // Catch any IO exception and report back to console
            LOGGER.info(e.toString());
        }
    } */
}
