package net.superricky.tpaplusplus.config;

import net.minecraftforge.common.ForgeConfigSpec;

/**
 * A forge config that contains messages for every configurable message in the mod.
 * The reason why this is used instead of Minecraft's easier to use translations, is that translations must be installed on the client.
 * This is why we use a forge config here, to avoid the issue of having to install this mod on the client, keeping everything 100% server-side.
 */
public class Messages {
    private static final String DISTANCE_PLACEHOLDER_COMMENT = " Placeholders: \"${distance}\": \"The distance between the two players.\"";

    public static final ForgeConfigSpec.Builder BUILDER = new ForgeConfigSpec.Builder();
    public static final ForgeConfigSpec SPEC;

    // General TPA stuff
    public static final ForgeConfigSpec.ConfigValue<String> ERR_REQUEST_NOT_FOUND;
    public static final ForgeConfigSpec.ConfigValue<String> ERR_NO_SELF_TELEPORT;
    public static final ForgeConfigSpec.ConfigValue<String> ERR_ALREADY_SENT_TELEPORT_REQUEST;
    public static final ForgeConfigSpec.ConfigValue<String> ERR_RECEIVER_TP_DISABLED;
    public static final ForgeConfigSpec.ConfigValue<String> ERR_SENDER_TP_DISABLED;
    public static final ForgeConfigSpec.ConfigValue<String> SENDER_BLOCKED_RECEIVER;
    public static final ForgeConfigSpec.ConfigValue<String> RECEIVER_BLOCKED_SENDER;

    // /back
    public static final ForgeConfigSpec.ConfigValue<String> ERR_BACK_COMMAND_DISABLED;
    public static final ForgeConfigSpec.ConfigValue<String> ERR_DEATH_LOC_NOT_FOUND;
    public static final ForgeConfigSpec.ConfigValue<String> DEATH_BEING_TELEPORTED;
    public static final ForgeConfigSpec.ConfigValue<String> DEATH_TELEPORTED;

    // /tpadeny
    public static final ForgeConfigSpec.ConfigValue<String> RECEIVER_DENIES_TPA;
    public static final ForgeConfigSpec.ConfigValue<String> SENDER_GOT_DENIED_TPA;

    // /tpacancel
    public static final ForgeConfigSpec.ConfigValue<String> SENDER_CANCELS_TPA;
    public static final ForgeConfigSpec.ConfigValue<String> RECEIVER_GOT_CANCELLED_TPA;

    // /tpaaccept
    public static final ForgeConfigSpec.ConfigValue<String> RECEIVER_ACCEPTS_TPA;
    public static final ForgeConfigSpec.ConfigValue<String> SENDER_GOT_ACCEPTED_TPA;

    // /tpa
    public static final ForgeConfigSpec.ConfigValue<String> SENDER_SENT_TPA;
    public static final ForgeConfigSpec.ConfigValue<String> RECEIVER_GOT_TPA;

    // /tpahere
    public static final ForgeConfigSpec.ConfigValue<String> SENDER_SENT_TPAHERE;
    public static final ForgeConfigSpec.ConfigValue<String> RECEIVER_GOT_TPAHERE;

    // /tptoggle
    public static final ForgeConfigSpec.ConfigValue<String> TPTOGGLE_ENABLED;
    public static final ForgeConfigSpec.ConfigValue<String> TPTOGGLE_DISABLED;

    // /tpblock
    public static final ForgeConfigSpec.ConfigValue<String> CANNOT_BLOCK_SELF;
    public static final ForgeConfigSpec.ConfigValue<String> ALREADY_BLOCKED_PLAYER;
    public static final ForgeConfigSpec.ConfigValue<String> SENDER_BLOCKED_PLAYER;
    public static final ForgeConfigSpec.ConfigValue<String> PLAYER_BLOCKED_BY_SENDER;

    // /tpunblock
    public static final ForgeConfigSpec.ConfigValue<String> CANNOT_UNBLOCK_SELF;
    public static final ForgeConfigSpec.ConfigValue<String> HAVENT_BLOCKED_PLAYER;
    public static final ForgeConfigSpec.ConfigValue<String> SENDER_UNBLOCKED_PLAYER;
    public static final ForgeConfigSpec.ConfigValue<String> PLAYER_UNBLOCKED_BY_SENDER;

    // TPA Timeout Messages
    public static final ForgeConfigSpec.ConfigValue<String> SENDER_TPA_TIMEOUT;
    public static final ForgeConfigSpec.ConfigValue<String> RECEIVER_TPA_TIMEOUT;
    public static final ForgeConfigSpec.ConfigValue<String> SENDER_TPAHERE_TIMEOUT;
    public static final ForgeConfigSpec.ConfigValue<String> RECEIVER_TPAHERE_TIMEOUT;

    // Limitation Messages
    public static final ForgeConfigSpec.ConfigValue<String> ERR_TOO_FAR_EXECUTOR;
    public static final ForgeConfigSpec.ConfigValue<String> ERR_TOO_FAR_OTHER_PLAYER;
    public static final ForgeConfigSpec.ConfigValue<String> ERR_TOO_CLOSE_EXECUTOR;
    public static final ForgeConfigSpec.ConfigValue<String> ERR_TOO_CLOSE_OTHER_PLAYER;
    public static final ForgeConfigSpec.ConfigValue<String> ERR_DIFFERENT_DIMENSIONS_EXECUTOR;
    public static final ForgeConfigSpec.ConfigValue<String> ERR_DIFFERENT_DIMENSIONS_OTHER_PLAYER;

    // /tpaplusplus Messages
    public static final ForgeConfigSpec.ConfigValue<String> TPAPLUSPLUS_VERSION;
    public static final ForgeConfigSpec.ConfigValue<String> TPAPLUSPLUS_FORCE_RELOADING_CONFIG;
    public static final ForgeConfigSpec.ConfigValue<String> TPAPLUSPLUS_FORCE_RELOADED_CONFIG;
    public static final ForgeConfigSpec.ConfigValue<String> TPAPLUSPLUS_RELOADING_CONFIG;
    public static final ForgeConfigSpec.ConfigValue<String> TPAPLUSPLUS_RELOADED_CONFIG;
    public static final ForgeConfigSpec.ConfigValue<String> ERR_TPAPLUSPLUS_COLORS_REQUIRE_SIX_COLORS;
    public static final ForgeConfigSpec.ConfigValue<String> ERR_TPAPLUSPLUS_COLORS_CANNOT_BE_THE_SAME;
    public static final ForgeConfigSpec.ConfigValue<String> ERR_TPAPLUSPLUS_COLORS_INVALID_COLORS;
    public static final ForgeConfigSpec.ConfigValue<String> ERR_TPAPLUSPLUS_COLORS_INVALID_COLORS_EXAMPLES;
    public static final ForgeConfigSpec.ConfigValue<String> TPAPLUSPLUS_COLORS_SUCCESS;

    // Countdown Messages
    public static final ForgeConfigSpec.ConfigValue<String> WINDUP_TIME_REMAINING;
    public static final ForgeConfigSpec.ConfigValue<String> PLAYER_MOVED_DURING_WINDUP;
    public static final ForgeConfigSpec.ConfigValue<String> ERR_COMMAND_ON_COOLDOWN_MESSAGE;
    public static final ForgeConfigSpec.ConfigValue<String> ERR_PLAYER_ON_GLOBAL_COOLDOWN_MESSAGE;

    static {
        BUILDER.push("TPA++ Messages");
        BUILDER.comment(" Don't know how Minecrafts §<color code> formatting works? Check out: https://www.digminecraft.com/lists/color_list_pc.php");
        BUILDER.comment("\n Due to the nature of now Strings are immutable in java, all values in this configuration require a restart to take affect!");
        BUILDER.comment("\n ALL MESSAGES SUPPORT JAVA SYNTAX FORMATTING! If you say want to put a newline into the message, just enter \\n, or if you want to add double quotes into a message enter \\\"");

        ERR_REQUEST_NOT_FOUND = BUILDER.comment("\n The message for when your teleport request could not be found.")
                        .define("ERR_REQUEST_NOT_FOUND", "§cCould not find your teleport request!");

        ERR_NO_SELF_TELEPORT = BUILDER.comment("\n The message for when a player attempts to use /tpa or /tpahere on themselves")
                        .define("ERR_NO_SELF_TELEPORT", "§cYou can't send a teleport request to yourself!");

        ERR_ALREADY_SENT_TELEPORT_REQUEST = BUILDER.comment("\n The message that is sent to the sender of a TPA request, when they have already sent a teleport request to that player.")
                .comment(" Placeholders: \"${receiver_name}\": \"The name of the receiver you tried to send a teleport request to\"")
                .define("ALREADY_SENT_TELEPORT_REQUEST", "§cYou already sent ${receiver_name} a TPA request!");

        ERR_RECEIVER_TP_DISABLED = BUILDER.comment("\n The message sent to the sender of a TPA request when the receiver has /tptoggle on, preventing all outside teleport requests")
                .comment(" Placeholders: \"${receiverName}\": \"The name of the receiver that they are trying to teleport to.\"")
                .define("ERR_RECEIVER_TP_DISABLED", "§c${receiverName} has disabled outside teleport requests!");

        ERR_SENDER_TP_DISABLED = BUILDER.comment("\n The message sent to the sender of a TPA request when the sender (themselves) has /tptoggle on, preventing all outside teleport requests")
                .comment(" This has no affect if this is disabled in the config")
                .define("ERR_SENDER_TP_DISABLED", "§cYou have disabled outside teleport requests!");

        SENDER_BLOCKED_RECEIVER = BUILDER.comment("\n The message that is sent to the sender of a TPA request when the sender (themselves) has blocked the recipient")
                .comment(" Placeholders: \"${blocked_player}\": \"The name of the player who has been blocked by the sender\"")
                .define("SENDER_BLOCKED_RECEIVER", "§6You have §cblocked ${blocked_player}§6!");

        RECEIVER_BLOCKED_SENDER = BUILDER.comment("\n The message that is sent to the sender of a TPA request, when the receiver has blocked the sender")
                .comment(" Placeholders: \"${blocking_player}\": \"The name of the player who has blocked the sender\"")
                .define("RECEIVER_BLOCKED_SENDER", "§c${blocking_player} §6has §cblocked §6you!");

        BUILDER.push("/back");

        ERR_BACK_COMMAND_DISABLED = BUILDER.comment("\n The message for when /back has been disabled in the servers configuration file.")
                        .define("ERR_BACK_COMMAND_DISABLED", "§cThis command has been disabled by a server administrator.");

        ERR_DEATH_LOC_NOT_FOUND = BUILDER.comment("\n The message for when your latest death location could not be found.")
                        .define("ERR_DEATH_LOC_NOT_FOUND", "§cCould not find your latest death position!");

        DEATH_BEING_TELEPORTED = BUILDER.comment("\n The message for when you are being teleported to your latest death")
                        .define("DEATH_BEING_TELEPORTED", "§6Teleporting you to your §clatest death §6position...");

        DEATH_TELEPORTED = BUILDER.comment("\n The message for when you are teleported to your latest death")
                        .define("DEATH_TELEPORTED", "§6You have been teleported!");

        BUILDER.pop();
        BUILDER.push("/tpadeny");

        RECEIVER_DENIES_TPA = BUILDER.comment("\n The message that is sent to the player who executes /tpadeny")
                        .comment(" Placeholders: \"%s\": \"The senders name who got denied\"")
                        .define("RECEIVER_DENIES_TPA", "§6Denied teleport request from §c%s");

        SENDER_GOT_DENIED_TPA = BUILDER.comment("\n The message that is sent to the sender when the receiver of their request executes /tpadeny")
                        .comment(" Placeholders: \"%s\": \"The receiver's name who denied the TPA\"")
                        .define("SENDER_GOT_DENIED_TPA", "§6Your teleport request for §c%s §6was denied!");

        BUILDER.pop();
        BUILDER.push("/tpacancel");

        SENDER_CANCELS_TPA = BUILDER.comment("\n The message that is sent to the player who executes /tpacancel")
                        .comment(" Placeholders: \"%s\": \"The receiver's name who the sender cancelled the TPA request for\"")
                        .define("SENDER_CANCELS_TPA", "§6Cancelled teleport request for §c%s");

        RECEIVER_GOT_CANCELLED_TPA = BUILDER.comment("\n The message that is sent to the receiver when the sender executs /tpacancel")
                        .comment(" Placeholders: \"%s\": \"The sender's name who cancelled the request\"")
                        .define("RECEIVER_GOT_CANCELLED_TPA", "§6Your teleport request from §c%s §6was cancelled!");

        BUILDER.pop();
        BUILDER.push("/tpaaccept");

        RECEIVER_ACCEPTS_TPA = BUILDER.comment("\n The message that is sent to the player who executes /tpaaccept")
                .comment(" Placeholders: \"%s\": \"The sender's name who sent the request\"")
                .define("RECEIVER_ACCEPTS_TPA", "§6Accepted teleport request from §c%s");

        SENDER_GOT_ACCEPTED_TPA = BUILDER.comment("\n The message that is sent to the sender of the TPA request, when their TPA request is accepted")
                .comment(" Placeholders: \"%s\": \"The receiver's name who accepted the request\"")
                .define("SENDER_GOT_ACCEPTED_TPA", "§6Your teleport request for §c%s §6was accepted!");

        BUILDER.pop();
        BUILDER.push("/tpa");

        SENDER_SENT_TPA = BUILDER.comment("\n The message that is sent to the player that executes /tpa")
                .comment(" Placeholders: \"%s\": \"The receiver's name who got sent the TPA request\"")
                .define("SENDER_SENT_TPA", "§6Successfully sent teleport request to §c%s");

        RECEIVER_GOT_TPA = BUILDER.comment("\n The message that is sent to the receiver of a TPA request")
                .comment(" Placeholders: \"%s\": \"The sender's name who sent the TPA request\"")
                .define("RECEIVER_GOT_TPA", "§c%s §6wants to teleport to you!");

        BUILDER.pop();
        BUILDER.push("/tpahere");

        SENDER_SENT_TPAHERE = BUILDER.comment("\n The message that is sent to the player that executes /tpahere")
                        .comment(" Placeholders: \"%s\": \"The receiver's name who got sent the TPAHERE request\"")
                        .define("SENDER_SENT_TPAHERE", "§6Successfully sent teleport §chere §6request to §c%s");

        RECEIVER_GOT_TPAHERE = BUILDER.comment("\n The message that is sent to the receiver of a TPAHERE request")
                        .comment(" Placeholders: \"%s\": \"The sender's name who sent the TPAHERE request\"")
                        .define("RECEIVER_GOT_TPAHERE", "§c%s §6wants §cyou §6to teleport to §cthem!");

        BUILDER.pop();
        BUILDER.push("/tptoggle");

        TPTOGGLE_ENABLED = BUILDER.comment("\n The message that is sent to the player that executes /tptoggle, when their tptoggle is enabled")
                .define("TPTOGGLE_ENABLED", "§6You are §cnow §6automatically §cdenying §6outside §cteleport requests§6!");

        TPTOGGLE_DISABLED = BUILDER.comment("\n The message that is sent to the player that executes /tptoggle, when their tptoggle is disabled")
                .define("TPTOGGLE_DISABLED", "§6You are §cno longer §6automatically §cdenying §6outside §cteleport requests§6!");

        BUILDER.pop();
        BUILDER.push("/tpblock");

        CANNOT_BLOCK_SELF = BUILDER.comment("\n The message that is sent to the player that executes /tpblock, when they attempt to block themselves")
                .define("CANNOT_BLOCK_SELF", "§6You §ccannot §6block §cyourself§6!");

        ALREADY_BLOCKED_PLAYER = BUILDER.comment("\n The message that is sent to the player that executes /tpblock, when they attempt to block a player whom they have already blocked")
                .comment(" Placeholders: \"${blocked_player}\": \"The player who has already been blocked\"")
                .define("ALREADY_BLOCKED_PLAYER", "§6You have §calready §6blocked §c${blocked_player}§6!");

        SENDER_BLOCKED_PLAYER = BUILDER.comment("\n The message that is sent to the player that executes /tpblock, when their block was successful")
                .comment(" Placeholders: \"${blocked_player}\": \"The player who was unblocked\"")
                .define("SENDER_BLOCKED_PLAYER", "§6Successfully blocked §c${blocked_player}§6!");

        PLAYER_BLOCKED_BY_SENDER = BUILDER.comment("\n The message that is sent to the player that was blocked by the sender")
                .comment(" This has no affect when it is disabled in the config.")
                .comment(" Placeholders: \"${sender_name}\": \"The player who blocked the recipient\"")
                .define("PLAYER_BLOCKED_BY_SENDER", "§6You have been §cblocked §6by §c${sender_name}§6!");

        BUILDER.pop();
        BUILDER.push("/tpunblock Messages");

        CANNOT_UNBLOCK_SELF = BUILDER.comment("\n The message that is sent to the player that executes /tpunblock, when they attempt to unblock themselves")
                .define("CANNOT_UNBLOCK_SELF", "§6You §ccannot §6unblock §cyourself§6!");

        HAVENT_BLOCKED_PLAYER = BUILDER.comment("\n The message that is sent to the player that executes /tpunblock, when they attempt to unblock a player whom they have not already blocked")
                .comment(" Placeholders: \"${blocked_player}\": \"The player who hasn't been blocked\"")
                .define("HAVENT_BLOCKED_PLAYER", "§6You §chaven't §6blocked §c${blocked_player}");

        SENDER_UNBLOCKED_PLAYER = BUILDER.comment("\n The message that is sent to the player that executes /tpunblock, when their unblock was successful")
                .comment(" Placeholders: \"${unblocked_player}\": \"The player who was unblocked\"")
                .define("SENDER_UNBLOCKED_PLAYER", "§6Successfully unblocked §c${unblocked_player}§6!");

        PLAYER_UNBLOCKED_BY_SENDER = BUILDER.comment("\n The message that is sent to the player that was unblocked by the sender")
                .comment(" This has no affect when it is disabled in the config.")
                .comment(" Placeholders: \"${sender_name}\": \"The player who blocked the recipient\"")
                .define("PLAYER_UNBLOCKED_BY_SENDER", "§6You have been §cunblocked §6by §c${sender_name}§6!");

        BUILDER.pop();
        BUILDER.push("TPA Request Timeout");

        SENDER_TPA_TIMEOUT = BUILDER.comment("\n The message that is sent to the sender of a TPA request, when their TPA request expires")
                        .comment(" Placeholders: \"%s\": \"The receiver's name who was sent the TPA request\"")
                        .define("SENDER_TPA_TIMEOUT", "§6Your teleport request to §c%s §6timed out!");

        RECEIVER_TPA_TIMEOUT = BUILDER.comment("\n The message that is sent to the receiver of a TPA request, when their TPA request expires")
                        .comment(" Placeholders: \"%s\": \"The sender's name who sent the receiver the TPA request\"")
                        .define("RECEIVER_TPA_TIMEOUT", "§6Your teleport request from §c%s §6timed out!");

        SENDER_TPAHERE_TIMEOUT = BUILDER.comment("\n The message that is sent to the sender of a TPAHERE request, when their TPAHERE request expires")
                        .comment(" Placeholders: \"%s\": \"The receiver's name who was sent the TPAHERE request\"")
                        .define("SENDER_TPA_TIMEOUT", "§6Your teleport §chere §6request to §c%s §6timed out!");

        RECEIVER_TPAHERE_TIMEOUT = BUILDER.comment("\n The message that is sent to the receiver of a TPAHERE request, when their TPAHERE request expires")
                        .comment(" Placeholders: \"%s\": \"The sender's name who sent the receiver the TPAHERE request\"")
                        .define("RECEIVER_TPA_TIMEOUT", "§6Your teleport §chere §6request from §c%s §6timed out!");

        BUILDER.pop();
        BUILDER.push("Limitations");

        ERR_TOO_FAR_EXECUTOR = BUILDER.comment("\n The message displayed when the player accepting or sending a TPA request, is too far away.")
                .comment(DISTANCE_PLACEHOLDER_COMMENT)
                .comment("     \"${expectedDistance}\": \"The maximum distance players can be from eachother to teleport\"")
                .define("ERR_TOO_FAR_EXECUTOR", """
                            §cYou are too far! You are ${distance} blocks apart!
                            §cYou must be a maximum of ${expectedDistance} blocks apart!""");

        ERR_TOO_FAR_OTHER_PLAYER = BUILDER.comment("\n The message displayed to the other player, when the TPA request is too far away")
                .comment(DISTANCE_PLACEHOLDER_COMMENT)
                .comment("     \"${expectedDistance}\": \"The maximum distance players can be from eachother to teleport\"")
                .comment("     \"${otherPlayerName}\": \"The name of the executor.\"")
                .define("ERR_TOO_FAR_OTHER_PLAYER", """
                                §c${otherPlayerName} failed to teleport: You are too far! You are ${distance} blocks apart!
                                §cYou must be a maximum of ${expectedDistance} blocks apart!""");

        ERR_TOO_CLOSE_EXECUTOR = BUILDER.comment("\n The message displayed when the player accepting or sending a TPA request, is too close.")
                .comment(DISTANCE_PLACEHOLDER_COMMENT)
                .comment("     \"${expectedDistance}\": \"The minimum distance players can be from eachother to teleport\"")
                .define("ERR_TOO_CLOSE_EXECUTOR", """
                            §cYou are too close! You are ${distance} blocks apart!
                            §cYou must be a minimum of ${expectedDistance} blocks apart!""");

        ERR_TOO_CLOSE_OTHER_PLAYER = BUILDER.comment("\n The message displayed to the other player, when the TPA request is too close.")
                .comment(DISTANCE_PLACEHOLDER_COMMENT)
                .comment("     \"${expectedDistance}\": \"The minimum distance players can be from eachother to teleport\"")
                .define("ERR_TOO_CLOSE_OTHER_PLAYER", """
                                §c${otherPlayerName} failed to teleport: You are too close! You are ${distance} blocks apart!
                                §cYou must be a minimum of ${expectedDistance} blocks apart!""");

        ERR_DIFFERENT_DIMENSIONS_EXECUTOR = BUILDER.comment("\n The message displayed when the player accepting or sending a TPA request, is in another dimension")
                .comment(" Placeholders: \"${executorDimension}\": \"The name of the dimension the executor is in\"")
                .comment("     \"${otherPlayerDimension\": \"The name of the dimension the other player is in\"")
                .define("ERR_DIFFERENT_DIMENSIONS_EXECUTOR", """
                            §cYou must be in the same dimension!
                            §cYou are in: ${executorDimension}
                            §cThey are in: ${otherPlayerDimension}""");

        ERR_DIFFERENT_DIMENSIONS_OTHER_PLAYER = BUILDER.comment("\n The message displayed to the other player, when the TPA request is from another dimension.")
                .comment(" Placeholders: \"${executorDimension}\": \"The name of the dimension the executor is in\"")
                .comment("     \"${otherPlayerDimension\": \"The name of the dimension the other player is in\"")
                .define("ERR_DIFFERENT_DIMENSIONS_EXECUTOR", """
                            §cYou must be in the same dimension!
                            §cYou are in: ${otherPlayerDimension}
                            §cThey are in: ${executorDimension}""");

        BUILDER.pop();
        BUILDER.push("/tpaplusplus");

        TPAPLUSPLUS_VERSION = BUILDER.comment("\n The message displayed when someone enters /tpaplusplus version")
                .comment(" Placeholders: \"%s\": \"The version the mod is running\"")
                .define("TPAPLUSPLUS_VERSION", "§6You are running TPAPlusPlus version §c%s");

        TPAPLUSPLUS_FORCE_RELOADING_CONFIG = BUILDER.comment("\n The first message displayed when someone enters /tpaplusplus reload config -force")
                .define("TPAPLUSPLUS_FORCE_RELOADING_CONFIG", "§6Reloading §cConfiguration§6 and wiping old data (TPA request's, death locations)...");

        TPAPLUSPLUS_FORCE_RELOADED_CONFIG = BUILDER.comment(" The second message displayed when someone enters /tpaplusplus reload config -force")
                .define("TPAPLUSPLUS_FORCE_RELOADED_CONFIG", "§6Finished Reloading Configuration and wiping data!");

        TPAPLUSPLUS_RELOADING_CONFIG = BUILDER.comment("\n The first message displayed when someone enters /tpaplusplus reload config")
                .define("TPAPLUSPLUS_RELOADING_CONFIG", "§6Reloading §cConfiguration§6...");

        TPAPLUSPLUS_RELOADED_CONFIG = BUILDER.comment("\n The second message displayed when someone enters /tpaplusplus reload config")
                .define("TPAPLUSPLUS_RELOADED_CONFIG", "§6Finished Reloading Configuration!");

        ERR_TPAPLUSPLUS_COLORS_REQUIRE_SIX_COLORS = BUILDER.comment("\n The message displayed when someone runs \"/tpaplusplus refactor colors\" with not enough colors specified")
                .comment("Placeholders: \"%s\": \"The amount of colours that the player entered.\"")
                .define("ERR_TPAPLUSPLUS_COLORS_REQUIRE_SIX_COLORS", "§cIllegal Arguments! Required 6 colors, you entered: %s");

        ERR_TPAPLUSPLUS_COLORS_CANNOT_BE_THE_SAME = BUILDER.comment("\n The message displayed when someone runs \"/tpaplusplus refactor colors\" when the primary and secondary colours are the same")
                .define("ERR_TPAPLUSPLUS_COLORS_CANNOT_BE_THE_SAME", "§cThe main and secondary colours cannot be the same!");

        ERR_TPAPLUSPLUS_COLORS_INVALID_COLORS = BUILDER.comment("\n The message displayed when someone runs \"/tpaplusplus refactor colors\" with invalid colour codes")
                .comment("Placeholders: \"%s\": \"The string the user entered instead of a valid colour code.\"")
                .define("ERR_TPAPLUSPLUS_COLORS_INVALID_COLORS", "§cIllegal Arguments! Requires a valid Minecraft chat colour code, you entered: %s");

        ERR_TPAPLUSPLUS_COLORS_INVALID_COLORS_EXAMPLES = BUILDER.comment(" The second message displayed when someone runs \"/tpaplusplus refactor colors\" with invalid colour codes")
                .comment("Placeholders: \"%s\": \"A random valid colour code\"")
                .define("ERR_TPAPLUSPLUS_COLORS_INVALID_COLORS_EXAMPLES", "§cExamples: %s, %s, %s, %s, %s, %s");

        TPAPLUSPLUS_COLORS_SUCCESS = BUILDER.comment("\n The message displayed when someone runs \"/tpaplusplus refactor colors\" successfully!")
                .define("TPAPLUSPLUS_COLORS_SUCCESS", "§6Successfully §creformatted §6your messages! Your §ccolor-set §6has been §cmodified§6!");

        BUILDER.pop();
        BUILDER.push("Windups");

        WINDUP_TIME_REMAINING = BUILDER.comment("\n The message displayed during a windup for a command when it has been enabled in the config")
                .comment("Placeholders: \"${time}\": \"How much time is remaining.\"")
                .define("COUNTDOWN_TIME_REMAINING", "§c${time}§6...");

        PLAYER_MOVED_DURING_WINDUP = BUILDER.comment("\n The message displayed during a windup for a command when the player moves further than the allowed distance in the config")
                .comment("Placeholders: \"${command_used}\": \"The command that the player tried to execute, but they moved too far. This placeholder respects the command names set in the config.\"")
                .define("PLAYER_MOVED_DURING_WINDUP", "§cYou moved too much whilst trying to execute ${command_used}, so it was cancelled.");

        BUILDER.pop();
        BUILDER.push("Cooldowns");

        ERR_COMMAND_ON_COOLDOWN_MESSAGE = BUILDER.comment("\n The message displayed when a player tries to execute a command but it is currently on cooldown.")
                .comment("Placeholders: \"${command_used}\": \"The command that was on cooldown. This placeholder respects the command names set in the config.\"")
                .comment("              \"${time_remaining}\": \"How long is left on the cooldown (in seconds)\"")
                .define("ERR_COMMAND_ON_COOLDOWN_MESSAGE", "§c\"/${command_used}\" is currently on cooldown, please wait ${time_remaining}s!");

        ERR_PLAYER_ON_GLOBAL_COOLDOWN_MESSAGE = BUILDER.comment("\n The message displayed when a player tries to execute a command, but they are currently on cooldown.")
                .comment("Placeholders: \"${time_remaining}\": \"How long is left on the cooldown (in seconds)\"")
                .define("ERR_PLAYER_ON_GLOBAL_COOLDOWN_MESSAGE", "§cYou are currently on cooldown, please wait ${time_remaining}s before running another command!");

        BUILDER.pop(2);
        SPEC = BUILDER.build();
    }

    private Messages() {
    }
}
