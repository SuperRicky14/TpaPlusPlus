package net.superricky.tpaplusplus.quilt;

import fuzs.forgeconfigapiport.api.config.v2.ForgeConfigRegistry;
import net.minecraftforge.fml.config.ModConfig;
import net.superricky.tpaplusplus.TPAPlusPlus;
import net.superricky.tpaplusplus.fabriclike.TPAPlusPlusFabricLike;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import org.quiltmc.loader.api.ModContainer;
import org.quiltmc.qsl.base.api.entrypoint.ModInitializer;

public class TPAPlusPlusQuilt implements ModInitializer {
    @Override
    public void onInitialize(ModContainer mod) {
        // Instantiate messages configuration
        ForgeConfigRegistry.INSTANCE.register(TPAPlusPlus.MOD_ID, ModConfig.Type.COMMON, Messages.SPEC, TPAPlusPlus.MESSAGES_CONFIG_PATH);

        // Instantiate configuration
        ForgeConfigRegistry.INSTANCE.register(TPAPlusPlus.MOD_ID, ModConfig.Type.COMMON, Config.SPEC, TPAPlusPlus.CONFIG_PATH);

        TPAPlusPlusFabricLike.init();
    }
}
