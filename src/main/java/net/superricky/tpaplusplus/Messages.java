package net.superricky.tpaplusplus;

import net.minecraftforge.common.ForgeConfigSpec;

/**
 * A forge config that contains messages for every configurable message in the mod.
 * The reason why this is used instead of Minecraft's easier to use translations, is that translations must be installed on the client.
 * This is why we use a forge config here, to avoid the issue of having to install this mod on the client, keeping everything 100% server-side.
 */
public class Messages {
    public static final ForgeConfigSpec.Builder BUILDER = new ForgeConfigSpec.Builder();
    public static final ForgeConfigSpec SPEC;

    // General TPA stuff
    public static final ForgeConfigSpec.ConfigValue<String> ERR_REQUEST_NOT_FOUND;
    public static final ForgeConfigSpec.ConfigValue<String> ERR_NO_SELF_TELEPORT;

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

    // TPA Timeout Messages
    public static final ForgeConfigSpec.ConfigValue<String> SENDER_TPA_TIMEOUT;
    public static final ForgeConfigSpec.ConfigValue<String> RECEIVER_TPA_TIMEOUT;
    public static final ForgeConfigSpec.ConfigValue<String> SENDER_TPAHERE_TIMEOUT;
    public static final ForgeConfigSpec.ConfigValue<String> RECEIVER_TPAHERE_TIMEOUT;

    // /tpaplusplus Messages
    public static final ForgeConfigSpec.ConfigValue<String> TPAPLUSPLUS_VERSION;
    public static final ForgeConfigSpec.ConfigValue<String> TPAPLUSPLUS_RELOADING_EVERYTHING;
    public static final ForgeConfigSpec.ConfigValue<String> TPAPLUSPLUS_RELOADED_EVERYTHING;
    public static final ForgeConfigSpec.ConfigValue<String> TPAPLUSPLUS_RELOADING_MESSAGES;
    public static final ForgeConfigSpec.ConfigValue<String> TPAPLUSPLUS_RELOADED_MESSAGES;
    public static final ForgeConfigSpec.ConfigValue<String> TPAPLUSPLUS_FORCE_RELOADING_CONFIG;
    public static final ForgeConfigSpec.ConfigValue<String> TPAPLUSPLUS_FORCE_RELOADED_CONFIG;
    public static final ForgeConfigSpec.ConfigValue<String> TPAPLUSPLUS_RELOADING_CONFIG;
    public static final ForgeConfigSpec.ConfigValue<String> TPAPLUSPLUS_RELOADED_CONFIG;
    public static final ForgeConfigSpec.ConfigValue<String> ERR_TPAPLUSPLUS_COLORS_REQUIRE_SIX_COLORS;
    public static final ForgeConfigSpec.ConfigValue<String> ERR_TPAPLUSPLUS_COLORS_INVALID_COLORS;
    public static final ForgeConfigSpec.ConfigValue<String> ERR_TPAPLUSPLUS_COLORS_INVALID_COLORS_EXAMPLES;
    public static final ForgeConfigSpec.ConfigValue<String> TPAPLUSPLUS_COLORS_SUCCESS;

    static {
        BUILDER.push("TPA++ Messages");
        BUILDER.comment(" Don't know how Minecrafts §<color code> formatting works? Check out: https://www.digminecraft.com/lists/color_list_pc.php");

        BUILDER.comment("\n-------------------------General TPA stuff-------------------------");

        ERR_REQUEST_NOT_FOUND = BUILDER.comment("\n The message for when your teleport request could not be found.")
                        .define("ERR_REQUEST_NOT_FOUND", "§cCould not find your teleport request!");

        ERR_NO_SELF_TELEPORT = BUILDER.comment("\n The message for when a player attempts to use /tpa or /tpahere on themselves")
                        .define("ERR_NO_SELF_TELEPORT", "§cYou can't send a teleport request to yourself!");

        BUILDER.comment("\n-------------------------/back Messages-------------------------");

        ERR_BACK_COMMAND_DISABLED = BUILDER.comment("\n The message for when /back has been disabled in the servers configuration file.")
                        .define("ERR_BACK_COMMAND_DISABLED", "§cThis command has been disabled by a server administrator.");

        ERR_DEATH_LOC_NOT_FOUND = BUILDER.comment("\n The message for when your latest death location could not be found.")
                        .define("ERR_DEATH_LOC_NOT_FOUND", "§cCould not find your latest death position!");

        DEATH_BEING_TELEPORTED = BUILDER.comment("\n The message for when you are being teleported to your latest death")
                        .define("DEATH_BEING_TELEPORTED", "§6Teleporting you to your §clatest death §6position...");

        DEATH_TELEPORTED = BUILDER.comment("\n The message for when you are teleported to your latest death")
                        .define("DEATH_TELEPORTED", "§6You have been teleported!");

        BUILDER.comment("\n-------------------------/tpadeny Messages-------------------------");

        RECEIVER_DENIES_TPA = BUILDER.comment("\n The message that is sent to the player who executes /tpadeny")
                        .comment(" Placeholders: \"%s\": \"The senders name who got denied\"")
                        .define("RECEIVER_DENIES_TPA", "§6Denied teleport request from §c%s");

        SENDER_GOT_DENIED_TPA = BUILDER.comment("\n The message that is sent to the sender when the receiver of their request executes /tpadeny")
                        .comment(" Placeholders: \"%s\": \"The receiver's name who denied the TPA\"")
                        .define("SENDER_GOT_DENIED_TPA", "§6Your teleport request for §c%s §6was denied!");

        BUILDER.comment("\n-------------------------/tpacancel Messages-------------------------");

        SENDER_CANCELS_TPA = BUILDER.comment("\n The message that is sent to the player who executes /tpacancel")
                        .comment(" Placeholders: \"%s\": \"The receiver's name who the sender cancelled the TPA request for\"")
                        .define("SENDER_CANCELS_TPA", "§6Cancelled teleport request for §c%s");

        RECEIVER_GOT_CANCELLED_TPA = BUILDER.comment("\n The message that is sent to the receiver when the sender executs /tpacancel")
                        .comment(" Placeholders: \"%s\": \"The sender's name who cancelled the request\"")
                        .define("RECEIVER_GOT_CANCELLED_TPA", "§6Your teleport request from §c%s §6was cancelled!");

        BUILDER.comment("\n-------------------------/tpaaccept Messages-------------------------");

        RECEIVER_ACCEPTS_TPA = BUILDER.comment("\n The message that is sent to the player who executes /tpaaccept")
                        .comment(" Placeholders: \"%s\": \"The sender's name who sent the request\"")
                        .define("RECEIVER_ACCEPTS_TPA", "§6Accepted teleport request from §c%s");

        SENDER_GOT_ACCEPTED_TPA = BUILDER.comment("\n The message that is sent to the sender of the TPA request, when their TPA request is accepted")
                        .comment(" Placeholders: \"%s\": \"The receiver's name who accepted the request\"")
                        .define("SENDER_GOT_ACCEPTED_TPA", "§6Your teleport request for §c%s §6was accepted!");

        BUILDER.comment("\n-------------------------/tpa Messages-------------------------");

        SENDER_SENT_TPA = BUILDER.comment("\n The message that is sent to the player that executes /tpa")
                        .comment(" Placeholders: \"%s\": \"The receiver's name who got sent the TPA request\"")
                        .define("SENDER_SENT_TPA", "§6Successfully sent teleport request to §c%s");

        RECEIVER_GOT_TPA = BUILDER.comment("\n The message that is sent to the receiver of a TPA request")
                        .comment(" Placeholders: \"%s\": \"The sender's name who sent the TPA request\"")
                        .define("RECEIVER_GOT_TPA", "§c%s §6wants to teleport to you!");

        BUILDER.comment("\n-------------------------/tpahere Messages-------------------------");

        SENDER_SENT_TPAHERE = BUILDER.comment("\n The message that is sent to the player that executes /tpahere")
                        .comment(" Placeholders: \"%s\": \"The receiver's name who got sent the TPAHERE request\"")
                        .define("SENDER_SENT_TPA", "§6Successfully sent teleport §chere §6request to §c%s");

        RECEIVER_GOT_TPAHERE = BUILDER.comment("\n The message that is sent to the receiver of a TPAHERE request")
                        .comment(" Placeholders: \"%s\": \"The sender's name who sent the TPAHERE request\"")
                        .define("RECEIVER_GOT_TPA", "§c%s §6wants §cyou §6to teleport to §cthem!");

        BUILDER.comment("\n-------------------------TPA Request Timeout Messages-------------------------");

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

        BUILDER.comment("\n-------------------------/tpaplusplus Messages-------------------------");

        TPAPLUSPLUS_VERSION = BUILDER.comment("\n The message displayed when someone enters /tpaplusplus version")
                .comment(" Placeholders: \"%s\": \"The version the mod is running\"")
                .define("TPAPLUSPLUS_VERSION", "§6You are running TPAPlusPlus version §c%s");

        TPAPLUSPLUS_RELOADING_EVERYTHING = BUILDER.comment("\n The first message displayed when someone enters /tpaplusplus reload")
                .define("TPAPLUSPLUS_RELOADING_EVERYTHING", "§6Reloading §cEverything...");

        TPAPLUSPLUS_RELOADED_EVERYTHING = BUILDER.comment(" The second message displayed when someone enters /tpaplusplus reload")
                .define("TPAPLUSPLUS_RELOADED_EVERYTHING", "§6Finished Reloading §cEverything!");

        TPAPLUSPLUS_RELOADING_MESSAGES = BUILDER.comment("\n The first message displayed when someone enters /tpaplusplus reload messages")
                .define("TPAPLUSPLUS_RELOADING_MESSAGES", "§6Reloading §cMessages§6...");

        TPAPLUSPLUS_RELOADED_MESSAGES = BUILDER.comment(" The second message displayed when someone enters /tpaplusplus reload messages")
                .define("TPAPLUSPLUS_RELOADED_MESSAGES", "§6Finished Reloading Messages! A §crestart §6is §crequired §6for the §cchanges §6to take §ceffect§6!");

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
                .define("TPAPLUSPLUS_COLORS_REQUIRE_SIX_COLORS", "§cIllegal Arguments! Required 6 colors, you entered: %s");

        ERR_TPAPLUSPLUS_COLORS_INVALID_COLORS = BUILDER.comment("\n The message displayed when someone runs \"/tpaplusplus refactor colors\" with invalid colour codes")
                .comment("Placeholders: \"%s\": \"The string the user entered instead of a valid colour code.\"")
                .define("ERR_TPAPLUSPLUS_COLORS_INVALID_COLORS", "§cIllegal Arguments! Requires a valid Minecraft chat colour code, you entered: %s");

        ERR_TPAPLUSPLUS_COLORS_INVALID_COLORS_EXAMPLES = BUILDER.comment(" The second message displayed when someone runs \"/tpaplusplus refactor colors\" with invalid colour codes")
                .comment("Placeholders: \"%s\": \"A random valid colour code\"")
                .define("ERR_TPAPLUSPLUS_COLORS_INVALID_COLORS_EXAMPLES", "§cExamples: %s, %s, %s, %s, %s, %s");

        TPAPLUSPLUS_COLORS_SUCCESS = BUILDER.comment("\n The message displayed when someone runs \"/tpaplusplus refactor colors\" successfully!")
                .define("TPAPLUSPLUS_COLORS_SUCCESS", "§6Successfully §creformatted §6your messages! Your §ccolor-set §6has been §cmodified§6!");

        BUILDER.pop();
        SPEC = BUILDER.build();
    }
}
