package net.superricky.tpaplusplus;

import net.minecraftforge.common.ForgeConfigSpec;

public class Config {
    public static final ForgeConfigSpec.Builder BUILDER = new ForgeConfigSpec.Builder();
    public static ForgeConfigSpec SPEC;

    public static ForgeConfigSpec.ConfigValue<Boolean> BACK_COMMAND_ENABLED;
    public static ForgeConfigSpec.ConfigValue<Integer> TPA_TIMEOUT_IN_SECONDS;
    public static ForgeConfigSpec.ConfigValue<Integer> TPA_ACCEPT_TIME_IN_SECONDS;
    public static ForgeConfigSpec.ConfigValue<Integer> TPA_COOLDOWN_IN_SECONDS;

    static {
        BUILDER.push("TPA++ Configuration");

        // TPA TIMEOUT
        TPA_TIMEOUT_IN_SECONDS = BUILDER.comment("How long until teleport requests expire (in seconds)")
                .comment("The default is 60 seconds ( 1 minute ), if you wish to disable this set this to 0")
                .defineInRange("TPA Timeout", 60, 0, Integer.MAX_VALUE);

        // TPA COOLDOWN
        /*TPA_COOLDOWN_IN_SECONDS = BUILDER.comment("How long it takes before another TPA request of the same type can be sent.")
                .comment("The default is 60 seconds ( 1 minute ) if you wish to disable this set this to 0")
                .defineInRange("TPA Cooldown", 60, 0, Integer.MAX_VALUE);*/

        // TPA ACCEPT TIME
        TPA_ACCEPT_TIME_IN_SECONDS = BUILDER.comment("How long it takes until a player is teleported via /tpaaccept")
                .comment("The default is 5 seconds, if you wish to disable this set this to 0")
                .defineInRange("TPA Accept Time", 5, 0, Integer.MAX_VALUE);

        // BACK COMMAND
        BACK_COMMAND_ENABLED = BUILDER.comment("Whether or not the /back system is enabled.")
                .comment("/back is the command that will teleport you to your latest death!")
                .define("Use /back", true);

        BUILDER.pop();
        SPEC = BUILDER.build();
    }
}
