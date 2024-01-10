package net.superricky.tpaplusplus.util.configuration;

import net.minecraftforge.common.ForgeConfigSpec;

public class Config {
    public static final ForgeConfigSpec.Builder BUILDER = new ForgeConfigSpec.Builder();
    public static final ForgeConfigSpec SPEC;

    public static final ForgeConfigSpec.ConfigValue<Boolean> BACK_COMMAND_ENABLED;
    public static final ForgeConfigSpec.ConfigValue<Integer> TPA_TIMEOUT_IN_SECONDS;
    public static final ForgeConfigSpec.ConfigValue<Integer> TPA_ACCEPT_TIME_IN_SECONDS;
    public static final ForgeConfigSpec.ConfigValue<Boolean> SEND_TELEPORT_REQUEST_COUNTDOWN_TO_BOTH_PLAYERS;
    public static final ForgeConfigSpec.ConfigValue<Double> ALLOWED_MOVEMENT_DURING_ACCEPT_COUNTDOWN;
    public static final ForgeConfigSpec.ConfigValue<Boolean> SEND_COUNTDOWN_MOVEMENT_CANCEL_TO_BOTH_PLAYERS;
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

    // Advanced Settings
    public static final ForgeConfigSpec.ConfigValue<Integer> ASYNC_GENERAL_TASKS_THREAD_POOL;
    public static final ForgeConfigSpec.ConfigValue<Integer> ASYNC_AUTOSAVE_THREAD_POOL;
    public static final ForgeConfigSpec.ConfigValue<Integer> AUTOSAVE_INTERVAL;

    static {
        BUILDER.push("TPA++ Configuration");
        BUILDER.comment(" Unlike the messages configuration, every value in this config can be changed and reloaded safely during runtime!");

        // TPA TIMEOUT
        TPA_TIMEOUT_IN_SECONDS = BUILDER.comment("\n How long until teleport requests expire (in seconds)")
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

        // TPA SEND ACCEPT TIME COUNTDOWN TO BOTH PLAYERS
        SEND_TELEPORT_REQUEST_COUNTDOWN_TO_BOTH_PLAYERS = BUILDER.comment("\n Whether or not to send the TPAAccept Countdown to both players.")
                        .define("Send Countdown To Both Players", true);

        // CANCEL TPA ACCEPT ON MOVE
        ALLOWED_MOVEMENT_DURING_ACCEPT_COUNTDOWN = BUILDER.comment("\n How much the player has to move to trigger a cancellation of the TPA accept countdown, due to moving.")
                .comment(" I left this here as an option, since this can be useful if you want to stop players from moving from a general area")
                .comment(" This range supports decimal values (e.g. 1.5), and is measured in blocks.")
                .comment(" Set this to 0 to allow players to move during the Teleport Accept Countdown, or to a very low number that is NOT 0 to prevent players from moving AT ALL, during the accept countdown.")
                .comment(" This uses the euclidean distance formula.")
                .comment(" Equal to: sqrt((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2) where x1, y1, and z1 are the coordinates of the player when they first accepted the request, and x2, y2, and z2 are the coordinates of the player every tick whilst the request is being accepted.")
                .comment(" This has NO EFFECT, if the TPA Accept Countdown is set to 0")
                .defineInRange("Allowed Movement During Accept Countdown", 1, 0, Double.MAX_VALUE);

        // SEND MESSAGE TO SENDER ON TPA ACCEPT
        SEND_COUNTDOWN_MOVEMENT_CANCEL_TO_BOTH_PLAYERS = BUILDER.comment("\n Whether to send a message to the sender when the receiver moves during a TPA accept countdown.")
                .define("Send Countdown Movement Cancel To Both Players", true);

        // ALLOW SENDERS TO SEND REQUESTS EVEN IF THEY HAVE TPTOGGLE ENABLED
        ALLOW_TPTOGGLED_PLAYERS_TO_SEND_REQUESTS = BUILDER.comment("\n Whether to allow players with /tptoggle enabled, to send a teleport request")
                .define("Allow TPToggled Players To Send Requests", false);

        // SEND MESSAGE TO THE PERSON BEING BLOCKED / UNBLOCKED
        SEND_BLOCKED_MESSAGES_TO_BOTH_PLAYERS = BUILDER.comment("\n Whether to send a message to the person being blocked / unblocked, when the sender blocks them.")
                        .define("Send Blocked Messages To Both Players", true);

        BUILDER.comment("\n-------------------------Commands-------------------------");
        BUILDER.comment(" This section of the config allows you to change the commands to whatever you please!");
        BUILDER.comment(" For example, you can change the /tpa command to /teleport-request, which means instead of entering /tpa players will have to enter /teleport-request");
        BUILDER.comment(" Command aliases (running the same command with one or more commands, for example you can make /tpa, /teleport-request and /tpasend all run the same commant ( /tpa )");
        BUILDER.comment(" It is NOT recommended to use anything other than ASCII characters here!");
        BUILDER.comment(" Modifying any of these commands requires a restart to take effect.");

        TPA_COMMAND_NAME = BUILDER.comment("\n The name of the /tpa command (what players run in chat)")
                .define("TPA_COMMAND_NAME", "tpa");

        TPAHERE_COMMAND_NAME = BUILDER.comment("\n The name of the /tpahere command (what players run in chat)")
                .define("TPAHERE_COMMAND_NAME", "tpahere");

        TPAACCEPT_COMMAND_NAME = BUILDER.comment("\n The name of the /tpaaccept command (what players run in chat)")
                .define("TPAACCEPT_COMMAND_NAME", "tpaaccept");

        TPADENY_COMMAND_NAME = BUILDER.comment("\n The name of the /tpadeny command (what players run in chat)")
                .define("TPADENY_COMMAND_NAME", "tpadeny");

        TPACANCEL_COMMAND_NAME = BUILDER.comment("\n The name of the /tpacancel command (what players run in chat)")
                .define("TPACANCEL_COMMAND_NAME", "tpacancel");

        TPTOGGLE_COMMAND_NAME = BUILDER.comment("\n The name of the /tptoggle command (what players run in chat)")
                .define("TPTOGGLE_COMMAND_NAME", "tptoggle");

        TPBLOCK_COMMAND_NAME = BUILDER.comment("\n The name of the /tpblock command (what players run in chat)")
                .define("TPBLOCK_COMMAND_NAME", "tpblock");

        TPUNBLOCK_COMMAND_NAME = BUILDER.comment("\n The name of the /tpunblock command (what players run in chat)")
                .define("TPUNBLOCK_COMMAND_NAME", "tpunblock");

        BACK_COMMAND_NAME = BUILDER.comment("\n The name of the /back command")
                .comment(" This has no affect if /back is not enabled in the configuration file.")
                .define("BACK_COMMAND_NAME", "back");

        BUILDER.comment("\n-------------------------Limitations-------------------------");
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
        ALLOW_INTER_DIMENSIONAL_TELEPORT = BUILDER.comment("\n Whether or not to allow sending TPA requests if the player(s) are not in the same dimension")
                .comment(" Set this to false if you wish to disable this limitation")
                .define("Allow Inter-Dimensional Teleport", true);

        // FURTHEST ALLOWED DISTANCE
        DISABLE_RANGE_CHECKS_INTER_DIMENSIONAL = BUILDER.comment("\n Whether or not to disable the range checks feature when teleporting inter-dimensionally")
                .comment(" This has no affect if \"Allow Inter-Dimensional Teleport\" is false, or if all range checks are disabled.")
                .comment(" TPAPlusPlus will automatically account for the nether's coordinate system ( 1 block in the nether is 8 blocks in the overworld! )")
                .comment(" Set this to false if you wish to disable this limitation")
                .define("Disable Inter Dimensional Range Checks", true);

        BUILDER.comment("\n-------------------------Advanced Settings-------------------------");
        BUILDER.comment(" WARNING: CHANGING THESE SETTINGS COULD SEVERELY DAMAGE YOUR SERVER, COMPUTER, OR CAUSE CORRUPTION OF DATA.");
        BUILDER.comment(" IT IS NOT RECOMMENDED TO CHANGE THESE VALUES FROM THE DEFAULTS UNLESS YOU KNOW EXACTLY WHAT YOU ARE DOING");
        BUILDER.comment(" Most options here have already been optimized for most systems, I doubt you will experience significant performance gains by changing the options below.");

        // THREAD POOL FOR GENERAL ASYNC TASKS
        ASYNC_GENERAL_TASKS_THREAD_POOL = BUILDER.comment("\n How many threads to use for general async tasks in the mod, such as the TPA Accept Countdown, or the TPA Timeout Timer.")
                .defineInRange("ASYNC_GENERAL_TASKS_THREAD_POOL", 1, 0, Integer.MAX_VALUE);

        // THREAD POOL FOR AUTO-SAVING
        ASYNC_AUTOSAVE_THREAD_POOL = BUILDER.comment("\n How many threads to use for asynchronous autosaving of data in the mod.")
                .comment(" Changing this setting will only provide negligible performance gains, even if you are using a very short autosave interval, this is because everything is synchronized between threads, preventing more than one thread from accessing it at a time.")
                .defineInRange("ASYNC_AUTOSAVE_THREAD_POOL", 1, 0, Integer.MAX_VALUE);

        // AUTO-SAVE INTERVAL
        AUTOSAVE_INTERVAL = BUILDER.comment("\n How long (in seconds) between autosaves, if you experience data loss, set this number lower.")
                .defineInRange("AUTOSAVE_INTERVAL", 300, 0, Integer.MAX_VALUE);

        BUILDER.pop();
        SPEC = BUILDER.build();
    }

    private Config() {
    }
}
