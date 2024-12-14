package net.superricky.tpaplusplus.fabric;

import net.fabricmc.api.ModInitializer;

import net.superricky.tpaplusplus.TpaPlusPlus;

public final class TpaPlusPlusFabric implements ModInitializer {
    @Override
    public void onInitialize() {
        // This code runs as soon as Minecraft is in a mod-load-ready state.
        // However, some things (like resources) may still be uninitialized.
        // Proceed with mild caution.

        // Run our common setup.
        TpaPlusPlus.init();
    }
}
