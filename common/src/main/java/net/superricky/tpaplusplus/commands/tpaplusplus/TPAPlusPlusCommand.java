package net.superricky.tpaplusplus.commands.tpaplusplus;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.arguments.StringArgumentType;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.superricky.tpaplusplus.TPAPlusPlus;
import net.superricky.tpaplusplus.commands.back.DeathHelper;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.network.UpdateCheckKt;
import net.superricky.tpaplusplus.requests.RequestHelper;
import net.superricky.tpaplusplus.util.MsgFmt;
import net.superricky.tpaplusplus.windupcooldown.windup.WindupWatcherKt;

import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

public class TPAPlusPlusCommand {
    private static final String FORCE_FLAG_NAME = "-force";

    public static void onRegisterCommandEvent(CommandDispatcher<CommandSourceStack> dispatcher) {
        dispatcher.register(literal("tpaplusplus")
                .executes(context -> version(context.getSource()))
                .then(literal("refactor")
                        .requires(context -> context.hasPermission(Commands.LEVEL_OWNERS))
                        .then(literal("messages")
                                .then(literal("color-set")
                                        .then(argument("old-primary-color", StringArgumentType.string())
                                                .then(argument("old-secondary-color", StringArgumentType.string())
                                                        .then(argument("old-error-color", StringArgumentType.string())
                                                                .then(argument("new-primary-color", StringArgumentType.string())
                                                                        .then(argument("new-secondary-color", StringArgumentType.string())
                                                                                .then(argument("new-error-color", StringArgumentType.string())
                                                                                        .executes(context -> refactorColorSet(context.getSource(),
                                                                                                StringArgumentType.getString(context, "old-primary-color"),
                                                                                                StringArgumentType.getString(context, "old-secondary-color"),
                                                                                                StringArgumentType.getString(context, "old-error-color"),
                                                                                                StringArgumentType.getString(context, "new-primary-color"),
                                                                                                StringArgumentType.getString(context, "new-secondary-color"),
                                                                                                StringArgumentType.getString(context, "new-error-color")
                                                                                        )))))))))))
                .then(literal("version")
                        .requires(context -> context.hasPermission(Commands.LEVEL_OWNERS))
                        .executes(context -> version(context.getSource())))
                .then(literal("reload")
                        .requires(context -> context.hasPermission(Commands.LEVEL_OWNERS))
                        .executes(context -> reloadConfig(context.getSource(), false))
                        .then(literal(FORCE_FLAG_NAME)
                                .executes(context -> reloadConfig(context.getSource(), true)))
                        .then(literal("config")
                                .executes(context -> reloadConfig(context.getSource(), false))
                                .then(literal(FORCE_FLAG_NAME)
                                        .executes(context -> reloadConfig(context.getSource(), true)))))
                .then(literal("license")
                        .executes(context -> printLicense(context.getSource()))));
    }

    private TPAPlusPlusCommand() {
    }

    private static int printLicense(CommandSourceStack source) {
        source.sendSystemMessage(Component.literal("§6TPA++ License & Credits: §chttps://github.com/SuperRicky14/TpaPlusPlus/blob/master/LICENSE"));
        return 1;
    }

    private static int refactorColorSet(CommandSourceStack source, String... colorList) {
        if (colorList.length != 6) {
            source.sendFailure(Component.literal(String.format(Messages.ERR_TPAPLUSPLUS_COLORS_REQUIRE_SIX_COLORS.get(), Map.of("amount_of_colours_entered", colorList.length))));
            return 0;
        }

        for (String color : colorList) {
            if (!ConfigReformatter.isValidColor(color)) {
                source.sendFailure(Component.literal(MsgFmt.fmt(Messages.ERR_TPAPLUSPLUS_COLORS_INVALID_COLORS.get(), Map.of("invalid_color_code", color))));
                source.sendFailure(Component.literal(MsgFmt.fmt(Messages.ERR_TPAPLUSPLUS_COLORS_INVALID_COLORS_EXAMPLES.get(), Map.of("random_color_code", (Supplier<String>) ConfigReformatter::getRandomColorCode))));
                return 0;
            }
        }

        String oldMainColor = colorList[0].replace("&", "§").toLowerCase();
        String oldSecondaryColor = colorList[1].replace("&", "§").toLowerCase();
        String oldErrorColor = colorList[2].replace("&", "§").toLowerCase();
        String newMainColor = colorList[3].replace("&", "§").toLowerCase();
        String newSecondaryColor = colorList[4].replace("&", "§").toLowerCase();
        String newErrorColor = colorList[5].replace("&", "§").toLowerCase();

        if (oldMainColor.equals(oldSecondaryColor) || newMainColor.equals(newSecondaryColor)) {
            source.sendFailure(Component.literal(Messages.ERR_TPAPLUSPLUS_COLORS_CANNOT_BE_THE_SAME.get()));
            return 0;
        }

        ConfigReformatter.updateColorsAndSave(ConfigReformatter.loadRawConfig(),
                oldMainColor,
                newMainColor,
                oldSecondaryColor,
                newSecondaryColor,
                oldErrorColor,
                newErrorColor);

        source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_COLORS_SUCCESS.get()));

        return 1;
    }

    private static int version(CommandSourceStack source) {
        source.sendSystemMessage(Component.literal(MsgFmt.fmt(Messages.TPAPLUSPLUS_VERSION.get(), Map.of("mod_version", TPAPlusPlus.MOD_VERSION)))); // send the mod's version to the command executor
        source.sendSystemMessage(Component.literal("§6Checking for updates..."));

        final Entity executor = source.getEntity();

        if (executor instanceof ServerPlayer serverPlayer) {
            // User is running version check in-game
            UpdateCheckKt.executeVersionCheck(serverPlayer);
        } else if (Objects.isNull(executor)) {
            // User is running version check from console
            UpdateCheckKt.executeVersionCheckFromConsole();
        }

        return 1;
    }

    /**
     * Reloads everything, and optionally clears all data related to the mod.
     * We also "restart" our ScheduledExecutorService here to prevent ConcurrentModificationExceptions with Forge's Config System, which I don't believe is Thread Safe
     * Warnings for "SameReturnValue" are suppressed since this command always succeeds and returning 1 both times is intended behaviour.
     *
     * @param force whether to clear all TPA++ related memory (requests, deaths, windup data) or just soft reload without deleting teleport requests and death locations.
     */
    @SuppressWarnings("SameReturnValue")
    private static int reloadConfig(CommandSourceStack source, boolean force) {
        if (force) {
            source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_FORCE_RELOADING_CONFIG.get()));

            Config.SPEC.afterReload();

            RequestHelper.clearRequestSet();
            DeathHelper.clearDeathCoordinates();
            WindupWatcherKt.clearTrackedWindupData();

            source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_FORCE_RELOADED_CONFIG.get()));
            return 1;
        }
        source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_RELOADING_CONFIG.get()));

        Config.SPEC.afterReload();

        source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_RELOADED_CONFIG.get()));
        return 1;
    }
}
