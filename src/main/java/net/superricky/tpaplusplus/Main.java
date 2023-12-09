package net.superricky.tpaplusplus;

import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.config.ModConfig;
import net.superricky.tpaplusplus.teleport.Teleport;

import java.util.HashMap;

// The value here should match an entry in the META-INF/mods.toml file
@Mod(Main.MOD_ID)
public class Main {
    public static final String MOD_ID = "tpaplusplus";

    // The teleport request is our Teleport part, the time remaining (in ticks) is the Integer part.
    public static HashMap<Teleport, Integer> teleportRequests = new HashMap<>();
    public static HashMap<ServerPlayer, Vec3> playerDeathCoordinates = new HashMap<>();
    public Main() {
        MinecraftForge.EVENT_BUS.register(this);

        // Instantiate the mods configuration
        ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, Config.SPEC, "tpaplusplus-common.toml");
    }
}
