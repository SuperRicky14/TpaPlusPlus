package net.superricky.tpaplusplus;

import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.config.ModConfig;
import net.superricky.tpaplusplus.util.configuration.Config;
import net.superricky.tpaplusplus.util.configuration.Messages;

// The value here should match an entry in the META-INF/mods.toml file
@Mod(Main.MOD_ID)
public class Main {
    // Our mod id
    public static final String MOD_ID = "tpaplusplus";
    public static final String MOD_VERSION = "1.3-1.20.x-BETA-3";

    public Main() {
        MinecraftForge.EVENT_BUS.register(this);

        // Instantiate the mod's messages
        ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, Messages.SPEC, "tpaplusplus-messages.toml");

        // Instantiate the mod's configuration
        ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, Config.SPEC, "tpaplusplus-common.toml");
    }

    /**
     * Calculates the Euclidean distance between two points in 3-dimensional space.
     * Equal to: sqrt((x2 - x1)² + (y2 - y1)² + (z2 - z1)²)
     *
     * @param x1 the x-coordinate of the first point
     * @param y1 the y-coordinate of the first point
     * @param z1 the z-coordinate of the first point
     * @param x2 the x-coordinate of the second point
     * @param y2 the y-coordinate of the second point
     * @param z2 the z-coordinate of the second point
     * @return the Euclidean distance between the two points
     */
    public static double euclideanDistanceCalculator3D(double x1, double y1, double z1, double x2, double y2, double z2) {
        return Math.sqrt(Math.pow(x2 - x1, 2) +
                Math.pow(y2 - y1, 2) +
                Math.pow(z2 - z1, 2));
    }
}
