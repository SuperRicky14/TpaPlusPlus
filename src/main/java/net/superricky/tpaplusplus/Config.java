package net.superricky.tpaplusplus;

import net.minecraftforge.common.ForgeConfigSpec;

public class Config {
    public static final ForgeConfigSpec.Builder BUILDER = new ForgeConfigSpec.Builder();
    public static ForgeConfigSpec SPEC;

    public static ForgeConfigSpec.ConfigValue<Boolean> BACK_COMMAND_ENABLED;
    public static ForgeConfigSpec.ConfigValue<Integer> TPA_TIMEOUT_IN_SECONDS;
    //public static ForgeConfigSpec.ConfigValue<Boolean> SAVE_DEATH_COORDS_TO_DISK;
    //public static ForgeConfigSpec.ConfigValue<Boolean> SAVE_TELEPORT_REQUESTS_TO_DISK;

    static {
        BUILDER.push("TPA++ Configuration");

        TPA_TIMEOUT_IN_SECONDS = BUILDER.comment("How long until teleport requests expire (in seconds)")
                .comment("The default is 60 seconds ( 1 minute ), if you wish to disable this set this to a really big number ( no way to disable it currently )")
                .defineInRange("TPA Timeout", 60, 1, Integer.MAX_VALUE);

        BACK_COMMAND_ENABLED = BUILDER.comment("Whether or not the /back system is enabled.")
                .comment("/back is the command that will teleport you to your latest death!")
                .define("Use /back", true);

        /*
        SAVE_DEATH_COORDS_TO_DISK = BUILDER.comment("Whether or not death coordinates (used for the /back command) are saved across restarts, and on auto-save")
                .comment("Has no affect if /back is not enabled.")
                .define("Save death coords to disk", true);

        SAVE_TELEPORT_REQUESTS_TO_DISK = BUILDER.comment("Whether or not teleport requests (used for /tpa, /tpaaccept, etc) are saved across restarts, and on auto-save")
                .define("Save TPA Requests to disk", false);
         */

        BUILDER.pop();
        SPEC = BUILDER.build();
    }
}
