package _TESTING.superricky.tpaplusplus.test;

import _TESTING.superricky.tpaplusplus.test.teleport._Teleport;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.config.ModConfig;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

// The value here should match an entry in the META-INF/mods.toml file
@Mod(_Main.MOD_ID)
public class _Main {
    // Our mod id
    public static final String MOD_ID = "tpaplusplus";
    public static final String MOD_VERSION = "1.3-1.20.x-BLEEDING_EDGE";

    // The teleport request is our Teleport part, the time remaining (in ticks) is the Integer part.
    public static final Set<_Teleport> TELEPORT_REQUESTS = new HashSet<>();

    // Store every player's death coordinates in a hashmap.
    public static final HashMap<ServerPlayer, Vec3> playerDeathCoordinates = new HashMap<>();

    // Store every player's time left until teleporting in a hashmap.
    public static final Set<_Teleport> TELEPORTING_PLAYERS = new HashSet<>();

    public _Main() {
        MinecraftForge.EVENT_BUS.register(this);

        // Instantiate the mods configuration
        ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, _Config.SPEC, "tpaplusplus-common.toml");
    }
}
