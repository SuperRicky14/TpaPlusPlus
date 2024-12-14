package net.superricky.tpaplusplus.neoforge;

import net.neoforged.fml.common.Mod;

import net.superricky.tpaplusplus.ExampleMod;

@Mod(ExampleMod.MOD_ID)
public final class ExampleModNeoForge {
    public ExampleModNeoForge() {
        // Run our common setup.
        ExampleMod.init();
    }
}
