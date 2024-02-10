package net.superricky.tpaplusplus.forge;

import dev.architectury.platform.forge.EventBuses;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.config.ModConfig;
import net.superricky.tpaplusplus.TPAPlusPlus;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.superricky.tpaplusplus.util.configuration.Config;
import net.superricky.tpaplusplus.util.configuration.Messages;

@Mod(TPAPlusPlus.MOD_ID)
public class TPAPlusPlusForge {
    public TPAPlusPlusForge() {
        // Submit our event bus to let architectury register our content on the right time
        EventBuses.registerModEventBus(TPAPlusPlus.MOD_ID, FMLJavaModLoadingContext.get().getModEventBus());

        // Instantiate messages configuration
        ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, Messages.SPEC, TPAPlusPlus.MESSAGES_CONFIG_PATH);

        // Instantiate configuration
        ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, Config.SPEC, TPAPlusPlus.CONFIG_PATH);

        TPAPlusPlus.init();
    }
}
