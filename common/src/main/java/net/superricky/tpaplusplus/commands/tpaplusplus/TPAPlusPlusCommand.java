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
import net.superricky.tpaplusplus.config.formatters.MessageReformatter;
import net.superricky.tpaplusplus.io.AutosaveScheduler;
import net.superricky.tpaplusplus.network.UpdateCheckKt;
import net.superricky.tpaplusplus.requests.RequestHelper;
import net.superricky.tpaplusplus.timeout.TimeoutScheduler;
import net.superricky.tpaplusplus.windupcooldown.windup.AsyncWindup;
import net.superricky.tpaplusplus.windupcooldown.windup.WindupWatcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

public class TPAPlusPlusCommand {
    private static final String FORCE_PARAMETER = "-force";
    private static final String SCHEDULED_EXECUTOR_SERVICE_EXCEPTION_MESSAGE = "§4Failed to reload: An internal server error occurred, please check console for more information.";
    private static final Logger LOGGER = LoggerFactory.getLogger(TPAPlusPlus.MOD_ID);

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
                        .then(literal(FORCE_PARAMETER)
                                .executes(context -> reloadConfig(context.getSource(), true)))
                        .then(literal("config")
                                .executes(context -> reloadConfig(context.getSource(), false))
                                .then(literal(FORCE_PARAMETER)
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
            source.sendFailure(Component.literal(String.format(Messages.ERR_TPAPLUSPLUS_COLORS_REQUIRE_SIX_COLORS.get(), colorList.length)));
            return 0;
        }

        for (String color : colorList) {
            if (!MessageReformatter.isValidColor(color)) {
                source.sendFailure(Component.literal(String.format(Messages.ERR_TPAPLUSPLUS_COLORS_INVALID_COLORS.get(), color)));
                source.sendFailure(Component.literal(String.format(Messages.ERR_TPAPLUSPLUS_COLORS_INVALID_COLORS_EXAMPLES.get(), MessageReformatter.getRandomColorCode())));
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

        MessageReformatter.updateColorsAndSave(MessageReformatter.loadRawConfig(),
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
        source.sendSystemMessage(Component.literal(String.format(Messages.TPAPLUSPLUS_VERSION.get(), TPAPlusPlus.MOD_VERSION))); // send the mod's version to the command executor
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

    private static void logAndWarnTerminatedScheduledExecutorService(CommandSourceStack source, boolean executorServiceResult) {
        if (Boolean.FALSE.equals(executorServiceResult)) {
            source.sendSystemMessage(Component.literal("§eWARNING§6: §fScheduledExecutorService refused shutdown request, so it was terminated!"));
            LOGGER.warn("ScheduledExecutorService timed out, so it was terminated!");
        }
    }

    /**
     * Reloads everything, and optionally clears all data related to the mod.
     * We also "restart" our ScheduledExecutorService here to prevent ConcurrentModificationExceptions with Forge's Config System, which I don't believe is Thread Safe
     * @param force whether to clear EVERYTHING currently loaded into RAM, or just soft reload without deleting teleport requests and death locations.
     */
    private static int reloadConfig(CommandSourceStack source, boolean force) {
        try {
            logAndWarnTerminatedScheduledExecutorService(source, AsyncWindup.stopScheduledExecutorService());
            logAndWarnTerminatedScheduledExecutorService(source, AutosaveScheduler.stopScheduledExecutorService());
            logAndWarnTerminatedScheduledExecutorService(source, TimeoutScheduler.stopScheduledExecutorService());
            logAndWarnTerminatedScheduledExecutorService(source, WindupWatcher.stopScheduledExecutorService());
        }
        catch (IllegalStateException | InterruptedException e) {
            LOGGER.error(e.getMessage());
            source.sendFailure(Component.literal(SCHEDULED_EXECUTOR_SERVICE_EXCEPTION_MESSAGE));
            return 0;
        }
        if (force) {
            source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_FORCE_RELOADING_CONFIG.get()));
            Config.SPEC.afterReload();
            RequestHelper.clearRequestSet();
            DeathHelper.clearDeathCoordinates();
            WindupWatcher.clearTrackedWindupData();
            source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_FORCE_RELOADED_CONFIG.get()));
            try {
                AsyncWindup.reCreateScheduledExecutorService();
                AutosaveScheduler.reCreateScheduledExecutorService();
                TimeoutScheduler.reCreateScheduledExecutorService();
                WindupWatcher.reCreateScheduledExecutorService();
            }
            catch (IllegalStateException e) {
                LOGGER.error(e.getMessage());
                source.sendFailure(Component.literal(SCHEDULED_EXECUTOR_SERVICE_EXCEPTION_MESSAGE));
                return 0;
            }
            return 1;
        }
        source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_RELOADING_CONFIG.get()));
        Config.SPEC.afterReload();
        source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_RELOADED_CONFIG.get()));
        try {
            AsyncWindup.reCreateScheduledExecutorService();
            AutosaveScheduler.reCreateScheduledExecutorService();
            TimeoutScheduler.reCreateScheduledExecutorService();
            WindupWatcher.reCreateScheduledExecutorService();
        }
        catch (IllegalStateException e) {
            LOGGER.error(e.getMessage());
            source.sendFailure(Component.literal(SCHEDULED_EXECUTOR_SERVICE_EXCEPTION_MESSAGE));
            return 0;
        }
        return 1;
    }
}
