package net.superricky.tpaplusplus.fabric;

import fuzs.forgeconfigapiport.api.config.v2.ForgeConfigRegistry;
import net.minecraftforge.fml.config.ModConfig;
import net.superricky.tpaplusplus.TPAPlusPlus;
import net.fabricmc.api.ModInitializer;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;

public class TPAPlusPlusFabric implements ModInitializer {
    @Override
    public void onInitialize() {
        // Instantiate messages configuration
        ForgeConfigRegistry.INSTANCE.register(TPAPlusPlus.MOD_ID, ModConfig.Type.COMMON, Messages.SPEC, TPAPlusPlus.MESSAGES_CONFIG_PATH);

        // Instantiate configuration
        ForgeConfigRegistry.INSTANCE.register(TPAPlusPlus.MOD_ID, ModConfig.Type.COMMON, Config.SPEC, TPAPlusPlus.CONFIG_PATH);

        TPAPlusPlus.init();
    }
}
