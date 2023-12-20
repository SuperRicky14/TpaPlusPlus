package net.superricky.tpaplusplus;

import java.util.HashMap;
import java.util.Map;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.config.ModConfig;
import net.superricky.tpaplusplus.teleport.Teleport;

// The value here should match an entry in the META-INF/mods.toml file
@Mod(Main.MOD_ID)
public class Main {
    // Our mod id
    public static final String MOD_ID = "tpaplusplus";
    public static final String MOD_VERSION = "1.3-1.20.x-BLEEDING_EDGE-2.0";

    // The teleport request is our Teleport part, the time remaining (in ticks) is the Integer part.
    public static final Map<Teleport, Integer> teleportRequests = new HashMap<>();

    // Store every player's death coordinates in a hashmap.
    public static final Map<ServerPlayer, Vec3> playerDeathCoordinates = new HashMap<>();

    // Store every player's time left until teleporting in a hashmap.
    public static final Map<Teleport, Integer> playerTeleportTime = new HashMap<>();

    public Main() {
        MinecraftForge.EVENT_BUS.register(this);

        // Instantiate the mods configuration
        ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, Config.SPEC, "tpaplusplus-common.toml");
    }
}
