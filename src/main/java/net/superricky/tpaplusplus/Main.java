package net.superricky.tpaplusplus;

import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.config.ModConfig;

// The value here should match an entry in the META-INF/mods.toml file
@Mod(Main.MOD_ID)
public class Main {
    // Our mod id
    public static final String MOD_ID = "tpaplusplus";
    public static final String MOD_VERSION = "1.3-1.20.x-BETA";

    public Main() {
        MinecraftForge.EVENT_BUS.register(this);

        // Instantiate the mods messages
        ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, Messages.SPEC, "tpaplusplus-messages.toml");

        // Instantiate the mods configuration
        ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, Config.SPEC, "tpaplusplus-common.toml");

    }
}
