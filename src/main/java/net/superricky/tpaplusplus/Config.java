package net.superricky.tpaplusplus;

import net.minecraftforge.common.ForgeConfigSpec;

public class Config {
    public static final ForgeConfigSpec.Builder BUILDER = new ForgeConfigSpec.Builder();
    public static ForgeConfigSpec SPEC;

    public static ForgeConfigSpec.ConfigValue<Boolean> BACK_COMMAND_ENABLED;
    public static ForgeConfigSpec.ConfigValue<Integer> TPA_TIMEOUT_IN_SECONDS;

    static {
        BUILDER.push("TPA++ Configuration");

        TPA_TIMEOUT_IN_SECONDS = BUILDER.comment("How long until teleport requests expire (in seconds)")
                .comment("The default is 60 seconds ( 1 minute ), if you wish to disable this set this to a really big number ( no way to disable it currently )")
                .comment("Min: 1 Second | Max: 2147483647 Seconds")
                .defineInRange("TPA Timeout", 60, 1, 2147483647);

        BACK_COMMAND_ENABLED = BUILDER.comment("Whether or not the /back system is enabled.")
                .comment("/back is the command that will teleport you to your latest death!")
                .define("Use /back", true);

        BUILDER.pop();
        SPEC = BUILDER.build();
    }
}
