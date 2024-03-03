package net.superricky.tpaplusplus.config;

import net.minecraftforge.common.ForgeConfigSpec;

public class Config {
    public static final ForgeConfigSpec.Builder BUILDER = new ForgeConfigSpec.Builder();
    public static final ForgeConfigSpec SPEC;

    public static final ForgeConfigSpec.ConfigValue<Boolean> BACK_COMMAND_ENABLED;
    public static final ForgeConfigSpec.ConfigValue<Integer> TPA_TIMEOUT_IN_SECONDS;
    public static final ForgeConfigSpec.ConfigValue<Integer> TPA_ACCEPT_TIME_IN_SECONDS;
    public static final ForgeConfigSpec.ConfigValue<Boolean> ALLOW_TPTOGGLED_PLAYERS_TO_SEND_REQUESTS;
    public static final ForgeConfigSpec.ConfigValue<Boolean> SEND_BLOCKED_MESSAGES_TO_BOTH_PLAYERS;

    // Commands
    public static final ForgeConfigSpec.ConfigValue<String> TPA_COMMAND_NAME;
    public static final ForgeConfigSpec.ConfigValue<String> TPAHERE_COMMAND_NAME;
    public static final ForgeConfigSpec.ConfigValue<String> TPAACCEPT_COMMAND_NAME;
    public static final ForgeConfigSpec.ConfigValue<String> TPADENY_COMMAND_NAME;
    public static final ForgeConfigSpec.ConfigValue<String> TPACANCEL_COMMAND_NAME;
    public static final ForgeConfigSpec.ConfigValue<String> TPTOGGLE_COMMAND_NAME;
    public static final ForgeConfigSpec.ConfigValue<String> TPBLOCK_COMMAND_NAME;
    public static final ForgeConfigSpec.ConfigValue<String> TPUNBLOCK_COMMAND_NAME;
    public static final ForgeConfigSpec.ConfigValue<String> BACK_COMMAND_NAME;

    // Limitations
    public static final ForgeConfigSpec.ConfigValue<Double> FURTHEST_ALLOWED_DISTANCE;
    public static final ForgeConfigSpec.ConfigValue<Double> CLOSEST_ALLOWED_DISTANCE;
    public static final ForgeConfigSpec.ConfigValue<Boolean> ALLOW_INTER_DIMENSIONAL_TELEPORT;
    public static final ForgeConfigSpec.ConfigValue<Boolean> DISABLE_RANGE_CHECKS_INTER_DIMENSIONAL;

    // Windup
    public static final ForgeConfigSpec.ConfigValue<Integer> BACK_WINDUP;
    public static final ForgeConfigSpec.ConfigValue<Integer> ACCEPT_WINDUP;
    public static final ForgeConfigSpec.ConfigValue<Integer> DENY_WINDUP;
    public static final ForgeConfigSpec.ConfigValue<Integer> CANCEL_WINDUP;
    public static final ForgeConfigSpec.ConfigValue<Integer> SEND_WINDUP;
    public static final ForgeConfigSpec.ConfigValue<Integer> TOGGLE_WINDUP;
    public static final ForgeConfigSpec.ConfigValue<Integer> BLOCK_WINDUP;
    public static final ForgeConfigSpec.ConfigValue<Integer> UNBLOCK_WINDUP;

    // Windup Distance
    public static final ForgeConfigSpec.ConfigValue<Double> BACK_WINDUP_DISTANCE;
    public static final ForgeConfigSpec.ConfigValue<Double> ACCEPT_WINDUP_DISTANCE;
    public static final ForgeConfigSpec.ConfigValue<Double> DENY_WINDUP_DISTANCE;
    public static final ForgeConfigSpec.ConfigValue<Double> CANCEL_WINDUP_DISTANCE;
    public static final ForgeConfigSpec.ConfigValue<Double> SEND_WINDUP_DISTANCE;
    public static final ForgeConfigSpec.ConfigValue<Double> TOGGLE_WINDUP_DISTANCE;
    public static final ForgeConfigSpec.ConfigValue<Double> BLOCK_WINDUP_DISTANCE;
    public static final ForgeConfigSpec.ConfigValue<Double> UNBLOCK_WINDUP_DISTANCE;

    // Cooldown
    public static final ForgeConfigSpec.ConfigValue<Integer> BACK_COOLDOWN;
    public static final ForgeConfigSpec.ConfigValue<Integer> ACCEPT_COOLDOWN;
    public static final ForgeConfigSpec.ConfigValue<Integer> DENY_COOLDOWN;
    public static final ForgeConfigSpec.ConfigValue<Integer> CANCEL_COOLDOWN;
    public static final ForgeConfigSpec.ConfigValue<Integer> SEND_COOLDOWN;
    public static final ForgeConfigSpec.ConfigValue<Integer> TOGGLE_COOLDOWN;
    public static final ForgeConfigSpec.ConfigValue<Integer> BLOCK_COOLDOWN;
    public static final ForgeConfigSpec.ConfigValue<Integer> UNBLOCK_COOLDOWN;
    public static final ForgeConfigSpec.ConfigValue<Boolean> ONLY_START_COOLDOWN_ON_COMMAND_SUCCESS;

    // Advanced Settings
    public static final ForgeConfigSpec.ConfigValue<Integer> AUTOSAVE_INTERVAL;
    public static final ForgeConfigSpec.ConfigValue<Boolean> USE_NON_BLOCKING_ASYNC_TICK_LOOP;
    public static final ForgeConfigSpec.ConfigValue<Integer> ASYNC_TICK_LOOP_UPDATE_RATE;

    static {
        BUILDER.push("TPA++ Configuration");
        BUILDER.comment(" Unlike the messages configuration, nearly every value in this config can be changed and reloaded safely during runtime!");

        // TPA TIMEOUT
        TPA_TIMEOUT_IN_SECONDS = BUILDER.comment(" How long until teleport requests expire (in seconds)")
                .comment(" The default is 60 seconds ( 1 minute ), if you wish to disable this set this to 0")
                .defineInRange("TPA Timeout", 60, 0, Integer.MAX_VALUE);

        // TPA ACCEPT TIME
        TPA_ACCEPT_TIME_IN_SECONDS = BUILDER.comment("\n How long it takes until a player is teleported via /tpaaccept")
                .comment(" The default is 5 seconds, if you wish to disable this set this to 0")
                .defineInRange("TPA Accept Time", 5, 0, Integer.MAX_VALUE);

        // BACK COMMAND
        BACK_COMMAND_ENABLED = BUILDER.comment("\n Whether or not the /back system is enabled.")
                .comment(" /back is the command that will teleport you to your latest death!")
                .define("Use /back", true);

        // ALLOW SENDERS TO SEND REQUESTS EVEN IF THEY HAVE TPTOGGLE ENABLED
        ALLOW_TPTOGGLED_PLAYERS_TO_SEND_REQUESTS = BUILDER.comment("\n Whether to allow players with /tptoggle enabled, to send a teleport request")
                .define("Allow TPToggled Players To Send Requests", false);

        // SEND MESSAGE TO THE PERSON BEING BLOCKED / UNBLOCKED
        SEND_BLOCKED_MESSAGES_TO_BOTH_PLAYERS = BUILDER.comment("\n Whether to send a message to the person being blocked / unblocked, when the sender blocks them.")
                        .define("Send Blocked Messages To Both Players", true);

        BUILDER.push("Commands");
        BUILDER.comment(" This section of the config allows you to change the commands to whatever you please!");
        BUILDER.comment(" For example, you can change the /tpa command to /teleport-request, which means instead of entering /tpa players will have to enter /teleport-request");
        BUILDER.comment(" Command aliases (running the same command with one or more commands, for example you can make /tpa, /teleport-request and /tpasend all run the same commant ( /tpa )");
        BUILDER.comment(" It is NOT recommended to use anything other than ASCII characters here!");
        BUILDER.comment(" Modifying any of these commands requires a restart to take effect.");

        TPA_COMMAND_NAME = BUILDER.comment("\n The name of the /tpa command (what players run in chat)")
                .worldRestart().define("TPA_COMMAND_NAME", "tpa");

        TPAHERE_COMMAND_NAME = BUILDER.comment("\n The name of the /tpahere command (what players run in chat)")
                .worldRestart().define("TPAHERE_COMMAND_NAME", "tpahere");

        TPAACCEPT_COMMAND_NAME = BUILDER.comment("\n The name of the /tpaaccept command (what players run in chat)")
                .worldRestart().define("TPAACCEPT_COMMAND_NAME", "tpaaccept");

        TPADENY_COMMAND_NAME = BUILDER.comment("\n The name of the /tpadeny command (what players run in chat)")
                .worldRestart().define("TPADENY_COMMAND_NAME", "tpadeny");

        TPACANCEL_COMMAND_NAME = BUILDER.comment("\n The name of the /tpacancel command (what players run in chat)")
                .worldRestart().define("TPACANCEL_COMMAND_NAME", "tpacancel");

        TPTOGGLE_COMMAND_NAME = BUILDER.comment("\n The name of the /tptoggle command (what players run in chat)")
                .worldRestart().define("TPTOGGLE_COMMAND_NAME", "tptoggle");

        TPBLOCK_COMMAND_NAME = BUILDER.comment("\n The name of the /tpblock command (what players run in chat)")
                .worldRestart().define("TPBLOCK_COMMAND_NAME", "tpblock");

        TPUNBLOCK_COMMAND_NAME = BUILDER.comment("\n The name of the /tpunblock command (what players run in chat)")
                .worldRestart().define("TPUNBLOCK_COMMAND_NAME", "tpunblock");

        BACK_COMMAND_NAME = BUILDER.comment("\n The name of the /back command")
                .comment(" This has no affect if /back is not enabled in the configuration file.")
                .define("BACK_COMMAND_NAME", "back");

        BUILDER.pop();
        BUILDER.push("Limitations");
        BUILDER.comment(" All limitations are DISABLED by default.");

        // FURTHEST ALLOWED DISTANCE
        FURTHEST_ALLOWED_DISTANCE = BUILDER.comment("\n How far away a player can be from another player in order to teleport")
                .comment(" Set this to 0 if you wish to disable this limitation")
                .defineInRange("Furthest Allowed Teleport Distance", 0, 0, Double.MAX_VALUE);

        // CLOSEST ALLOWED DISTANCE
        CLOSEST_ALLOWED_DISTANCE = BUILDER.comment("\n How close a player can be from another player in order to teleport")
                .comment(" Set this to 0 if you wish to disable this limitation")
                .defineInRange("Closest Allowed Teleport Distance", 0, 0, Double.MAX_VALUE);

        // CLOSEST ALLOWED DISTANCE
        ALLOW_INTER_DIMENSIONAL_TELEPORT = BUILDER.comment("\n Whether to allow sending TPA requests if the player(s) are not in the same dimension")
                .comment(" Set this to false if you wish to disable this limitation")
                .define("Allow Inter-Dimensional Teleport", true);

        // FURTHEST ALLOWED DISTANCE
        DISABLE_RANGE_CHECKS_INTER_DIMENSIONAL = BUILDER.comment("\n Whether to disable the range checks feature when teleporting inter-dimensionally")
                .comment(" This has no affect if \"Allow Inter-Dimensional Teleport\" is false, or if all range checks are disabled.")
                .comment(" TPAPlusPlus will automatically account for the nether's coordinate system ( 1 block in the nether is 8 blocks in the overworld! )")
                .comment(" Set this to false if you wish to disable this limitation")
                .define("Disable Inter Dimensional Range Checks", true);

        BUILDER.pop();
        BUILDER.push("Windups");
        BUILDER.push("Delay");
        BUILDER.comment(" This section of the config controls how long things like commands take to execute.");
        BUILDER.comment(" This is measured in seconds.");
        BUILDER.comment(" Set this to 0 if you wish to disable the countdown");

        BACK_WINDUP = BUILDER.comment("\n How long it takes for players to run /back.")
                        .defineInRange("Back Windup", 0, 0, Integer.MAX_VALUE);

        ACCEPT_WINDUP = BUILDER.comment("\n How long it takes for players to run /tpaaccept.")
                .defineInRange("Accept Windup", 0, 0, Integer.MAX_VALUE);

        DENY_WINDUP = BUILDER.comment("\n How long it takes for players to run /tpadeny.")
                .defineInRange("Deny Windup", 0, 0, Integer.MAX_VALUE);

        CANCEL_WINDUP = BUILDER.comment("\n How long it takes for players to run /tpacancel.")
                .defineInRange("Cancel Windup", 0, 0, Integer.MAX_VALUE);

        SEND_WINDUP = BUILDER.comment("\n How long it takes for players to run /tpa or /tpahere.")
                .defineInRange("Send Windup", 0, 0, Integer.MAX_VALUE);

        TOGGLE_WINDUP = BUILDER.comment("\n How long it takes for players to run /tptoggle.")
                .defineInRange("Toggle Windup", 0, 0, Integer.MAX_VALUE);

        BLOCK_WINDUP = BUILDER.comment("\n How long it takes for players to run /tpblock.")
                .defineInRange("Block Windup", 0, 0, Integer.MAX_VALUE);

        UNBLOCK_WINDUP = BUILDER.comment("\n How long it takes for players to run /tpunblock.")
                .defineInRange("Unblock Windup", 0, 0, Integer.MAX_VALUE);

        BUILDER.pop();
        BUILDER.push("Distance");
        BUILDER.comment(" This section of the config controls how far away people can be, from the position that they executed each command.");
        BUILDER.comment(" This is measured in blocks.");
        BUILDER.comment(" Set this to 0 if you want players to not be able to move at all during a countdown, or to -1 to completely disable this feature and allow them to move around freely during a windup.");
        BUILDER.comment(" WARNING: SETTING THESE VALUES IN-BETWEEN 0 AND -1 WILL CAUSE THEIR RESPECTIVE COMMAND TO BE UNUSABLE.");

        BACK_WINDUP_DISTANCE = BUILDER.comment("\n How far away players can be from the position where they ran /back.")
                .defineInRange("Back Windup Distance", 0, -1, Double.MAX_VALUE);

        ACCEPT_WINDUP_DISTANCE = BUILDER.comment("\n How far away players can be from the position where they ran /tpaaccept.")
                .defineInRange("Accept Windup Distance", 0, -1, Double.MAX_VALUE);

        DENY_WINDUP_DISTANCE = BUILDER.comment("\n How far away players can be from the position where they ran /tpadeny.")
                .defineInRange("Deny Windup Distance", 0, -1, Double.MAX_VALUE);

        CANCEL_WINDUP_DISTANCE = BUILDER.comment("\n How far away players can be from the position where they ran /tpacancel.")
                .defineInRange("Cancel Windup Distance", 0, -1, Double.MAX_VALUE);

        SEND_WINDUP_DISTANCE = BUILDER.comment("\n How far away players can be from the position where they ran /tpa or /tpahere.")
                .defineInRange("Send Windup Distance", 0, -1, Double.MAX_VALUE);

        TOGGLE_WINDUP_DISTANCE = BUILDER.comment("\n How far away players can be from the position where they ran /tptoggle.")
                .defineInRange("Toggle Windup Distance", 0, -1, Double.MAX_VALUE);

        BLOCK_WINDUP_DISTANCE = BUILDER.comment("\n How far away players can be from the position where they ran /tpblock.")
                .defineInRange("Block Windup Distance", 0, -1, Double.MAX_VALUE);

        UNBLOCK_WINDUP_DISTANCE = BUILDER.comment("\n How far away players can be from the position where they ran /tpunblock.")
                .defineInRange("Unblock Windup Distance", 0, -1, Double.MAX_VALUE);

        BUILDER.pop(2);

        BUILDER.push("Cooldowns");
        BUILDER.comment(" This section of the config controls the cooldown (how long the player must wait) after a command executes.");
        BUILDER.comment(" This is measured in seconds.");
        BUILDER.comment(" Set this to 0 if you wish to disable the cooldown");

        BACK_COOLDOWN = BUILDER.comment("\n How long it takes for players to run /back.")
                .defineInRange("Back Cooldown", 0, 0, Integer.MAX_VALUE);

        ACCEPT_COOLDOWN = BUILDER.comment("\n How long it takes for players to run /tpaaccept.")
                .defineInRange("Accept Cooldown", 0, 0, Integer.MAX_VALUE);

        DENY_COOLDOWN = BUILDER.comment("\n How long it takes for players to run /tpadeny.")
                .defineInRange("Deny Cooldown", 0, 0, Integer.MAX_VALUE);

        CANCEL_COOLDOWN = BUILDER.comment("\n How long it takes for players to run /tpacancel.")
                .defineInRange("Cancel Cooldown", 0, 0, Integer.MAX_VALUE);

        SEND_COOLDOWN = BUILDER.comment("\n How long it takes for players to run /tpa or /tpahere.")
                .defineInRange("Send Cooldown", 0, 0, Integer.MAX_VALUE);

        TOGGLE_COOLDOWN = BUILDER.comment("\n How long it takes for players to run /tptoggle.")
                .defineInRange("Toggle Cooldown", 0, 0, Integer.MAX_VALUE);

        BLOCK_COOLDOWN = BUILDER.comment("\n How long it takes for players to run /tpblock.")
                .defineInRange("Block Cooldown", 0, 0, Integer.MAX_VALUE);

        UNBLOCK_COOLDOWN = BUILDER.comment("\n How long it takes for players to run /tpunblock.")
                .defineInRange("Unblock Cooldown", 0, 0, Integer.MAX_VALUE);

        ONLY_START_COOLDOWN_ON_COMMAND_SUCCESS = BUILDER.comment("\n Whether to start the cooldown if the command executes successfully (e.g. at the end of a windup)")
                .comment(" Setting this value to true will only start the cooldown if the command runs successfully.")
                .comment(" Setting this value to false will start the countdown before starting a windup.")
                .define("Only Start Countdown On Command Success", true);

        BUILDER.pop();
        BUILDER.push("Advanced Settings");
        BUILDER.comment(" WARNING: These options are related to asynchronous operations. Modifying these from the defaults has a small chance to corrupt data.");
        BUILDER.comment(" WARNING: It is only recommended to change these values from their defaults if you know how they work internally.");
        BUILDER.comment(" Most options here have already been optimized for most systems, I doubt you will experience significant performance gains by changing the options below, unless you have an insane amount of players connected, in that case boosting the thread counts should help a bit.");
        BUILDER.comment(" Modifying these options may require a restart of the game.");

        // AUTO-SAVE INTERVAL
        AUTOSAVE_INTERVAL = BUILDER.comment("\n How long (in seconds) between autosaves, if you experience data loss, set this number lower.")
                .defineInRange("AUTOSAVE_INTERVAL", 300, 1, Integer.MAX_VALUE);

        USE_NON_BLOCKING_ASYNC_TICK_LOOP = BUILDER.comment("\n Whether to use an async tick loop for TPA++, that runs alongside the main thread.")
                .comment(" Operations that run synchronously with the main thread, are usually extremely inexpensive to run.")
                .comment(" Only enable this if you have an insane amount of players, or you have noticed performance problems with running it synchronously.")
                .worldRestart().define("USE_NON_BLOCKING_ASYNC_TICK_LOOP", false);

        ASYNC_TICK_LOOP_UPDATE_RATE = BUILDER.comment("\n How often (per second) to update the tick loop. Changing this value will NOT make the game tick run faster, it will only make TPA++ more sensitive and responsive to things (since it will be updated faster).")
                .comment(" For example, a value of 20 will cause TPA++ to update 20 times a second (this is also Minecraft's Tick Rate).")
                .comment(" It is completely okay to lower this value, even to something low like 4 if you have alot of players.")
                .worldRestart().defineInRange("ASYNC_TICK_LOOP_UPDATE_RATE", 20, 1, Integer.MAX_VALUE);

        BUILDER.pop(2);
        SPEC = BUILDER.build();
    }

    private Config() {
    }
}
