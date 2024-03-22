package net.superricky.tpaplusplus;

import dev.architectury.event.events.common.CommandRegistrationEvent;
import dev.architectury.event.events.common.EntityEvent;
import dev.architectury.event.events.common.LifecycleEvent;
import dev.architectury.event.events.common.TickEvent;
import net.superricky.tpaplusplus.commands.accept.TPAAcceptCommand;
import net.superricky.tpaplusplus.commands.back.BackCommand;
import net.superricky.tpaplusplus.commands.back.DeathHelper;
import net.superricky.tpaplusplus.commands.block.TPBlockCommand;
import net.superricky.tpaplusplus.commands.cancel.TPACancelCommand;
import net.superricky.tpaplusplus.commands.deny.TPADenyCommand;
import net.superricky.tpaplusplus.commands.send.TPACommand;
import net.superricky.tpaplusplus.commands.send.TPAHereCommand;
import net.superricky.tpaplusplus.commands.toggle.TPToggleCommand;
import net.superricky.tpaplusplus.commands.tpaplusplus.TPAPlusPlusCommand;
import net.superricky.tpaplusplus.commands.unblock.TPUnBlockCommand;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.formatters.MessageParser;
import net.superricky.tpaplusplus.io.ServerLifecycleHandler;
import net.superricky.tpaplusplus.timeout.RequestTimeoutEvent;
import net.superricky.tpaplusplus.timeout.TimeoutEventHandler;
import net.superricky.tpaplusplus.windupcooldown.CommandType;
import net.superricky.tpaplusplus.windupcooldown.cooldown.AsyncCooldown;
import net.superricky.tpaplusplus.windupcooldown.windup.WindupWatcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

public class TPAPlusPlus {
    public static final String MOD_ID = "tpaplusplus";
    public static final String MOD_VERSION = "1.5.1-1.20.x-BETA";
    private static final Logger LOGGER = LoggerFactory.getLogger(MOD_ID);

    public static final String CONFIG_PATH = "tpaplusplus-config.toml";
    public static final String MESSAGES_CONFIG_PATH = "tpaplusplus-messages.toml";

    private static final String SWITCH_COMMAND_NAME_FAILURE_ERROR_MESSAGE = "Switch statement could not find the respective command!";

    public static void init() {
        LOGGER.info("INITIALIZING...");

        LOGGER.info("REGISTERING BACK COMMAND...");
        CommandRegistrationEvent.EVENT.register((dispatcher, registry, selection) -> BackCommand.onRegisterCommandEvent(dispatcher));
        LOGGER.info("REGISTERING TPAACCEPT COMMAND...");
        CommandRegistrationEvent.EVENT.register((dispatcher3, registry3, selection3) -> TPAAcceptCommand.onRegisterCommandEvent(dispatcher3));
        LOGGER.info("REGISTERING TPACANCEL COMMAND...");
        CommandRegistrationEvent.EVENT.register((dispatcher3, registry3, selection3) -> TPACancelCommand.onRegisterCommandEvent(dispatcher3));
        LOGGER.info("REGISTERING TPA COMMAND...");
        CommandRegistrationEvent.EVENT.register((dispatcher3, registry3, selection3) -> TPACommand.onRegisterCommandEvent(dispatcher3));
        LOGGER.info("REGISTERING TPADENY COMMAND...");
        CommandRegistrationEvent.EVENT.register((dispatcher4, registry4, selection4) -> TPADenyCommand.onRegisterCommandEvent(dispatcher4));
        LOGGER.info("REGISTERING TPAHERE COMMAND...");
        CommandRegistrationEvent.EVENT.register((dispatcher3, registry3, selection3) -> TPAHereCommand.onRegisterCommandEvent(dispatcher3));
        LOGGER.info("REGISTERING TPAPLUSPLUS COMMAND...");
        CommandRegistrationEvent.EVENT.register((dispatcher2, registry2, selection2) -> TPAPlusPlusCommand.onRegisterCommandEvent(dispatcher2));
        LOGGER.info("REGISTERING TPBLOCK COMMAND...");
        CommandRegistrationEvent.EVENT.register((dispatcher1, registry1, selection1) -> TPBlockCommand.onRegisterCommandEvent(dispatcher1));
        LOGGER.info("REGISTERING TPTOGGLE COMMAND...");
        CommandRegistrationEvent.EVENT.register((dispatcher1, registry1, selection1) -> TPToggleCommand.onRegisterCommandEvent(dispatcher1));
        LOGGER.info("REGISTERING TPUNBLOCK COMMAND...");
        CommandRegistrationEvent.EVENT.register((dispatcher, registry, selection) -> TPUnBlockCommand.onRegisterCommandEvent(dispatcher));

        LOGGER.info("REGISTERING \"LifecycleEvent.SERVER_STARTED\"...");
        LifecycleEvent.SERVER_STARTED.register(state -> ServerLifecycleHandler.onServerStart());
        LOGGER.info("REGISTERING \"LifecycleEvent.SERVER_STOPPING\"...");
        LifecycleEvent.SERVER_STOPPING.register(state -> ServerLifecycleHandler.onServerStop());
        LOGGER.info("REGISTERING \"LifecycleEvent.LIVING_DEATH\"...");
        EntityEvent.LIVING_DEATH.register((deadEntity, source) -> DeathHelper.onDeath(deadEntity));

        LOGGER.info("REGISTERING \"RequestTimeoutEvent\"...");
        RequestTimeoutEvent.EVENT.register(TimeoutEventHandler::onTimeoutEvent);

        if (Config.USE_NON_BLOCKING_ASYNC_TICK_LOOP.get()) {
            LOGGER.warn("USING EXPERIMENTAL NON BLOCKING TICK LOOP");
            LOGGER.info(MessageParser.enhancedFormatter("INITIALIZING TICK LOOP WITH RATE OF ${tick_rate}...", Map.of("tick_rate", Config.ASYNC_TICK_LOOP_UPDATE_RATE.get())));
            WindupWatcher.startAsyncTickLoop(Config.ASYNC_TICK_LOOP_UPDATE_RATE.get());
        } else {
            LOGGER.info("USING SYNCHRONOUS TICK LOOP");
            LOGGER.info("REGISTERING \"TickEvent.SERVER_POST\"...");
            TickEvent.SERVER_POST.register(server -> WindupWatcher.watchWindupDataPosition());

            try {
                logAndWarnTerminatedScheduledExecutorService(WindupWatcher.deInstantiateScheduledExecutorService());
            } catch (InterruptedException e) {
                LOGGER.error("Failed to deInstantiate the ScheduledExecutorService for the Non-Blocking Async Tick Loop. You can keep playing, it will just stay loaded into RAM.");
            }
        }

        LOGGER.info("LOADING COOLDOWN LOOP...");
        AsyncCooldown.instantiateCooldownLoop();

        LOGGER.info("...INITIALIZATION COMPLETE");
    }

    private static void logAndWarnTerminatedScheduledExecutorService(boolean executorServiceResult) {
        if (Boolean.FALSE.equals(executorServiceResult)) {
            LOGGER.warn("The Non-Blocking tick loop's ScheduledExecutorService timed out, so it was terminated!");
        }
    }

    public static double distance3D(double x1, double y1, double z1, double x2, double y2, double z2) {
        return Math.sqrt(Math.pow(x2 - x1, 2) +
                Math.pow(y2 - y1, 2) +
                Math.pow(z2 - z1, 2));
    }

    public static String getCommandNameFromType(CommandType commandType, boolean isHereRequest) {
        switch (commandType) {
            case BACK -> {
                return Config.BACK_COMMAND_NAME.get();
            }
            case ACCEPT -> {
                return Config.TPAACCEPT_COMMAND_NAME.get();
            }
            case DENY -> {
                return Config.TPADENY_COMMAND_NAME.get();
            }
            case CANCEL -> {
                return Config.TPACANCEL_COMMAND_NAME.get();
            }
            case SEND -> {
                if (Boolean.TRUE.equals(isHereRequest)) {
                    // Sent request is a here-request
                    return Config.TPAHERE_COMMAND_NAME.get();
                } else {
                    // Sent request is a normal request
                    return Config.TPA_COMMAND_NAME.get();
                }
            }
            case BLOCK -> {
                return Config.TPBLOCK_COMMAND_NAME.get();
            }
            case TOGGLE -> {
                return Config.TPTOGGLE_COMMAND_NAME.get();
            }
            case UNBLOCK -> {
                return Config.TPUNBLOCK_COMMAND_NAME.get();
            }
        }
        LOGGER.error(SWITCH_COMMAND_NAME_FAILURE_ERROR_MESSAGE);
        throw new IllegalStateException(SWITCH_COMMAND_NAME_FAILURE_ERROR_MESSAGE);
    }

    public static String getCommandNameFromType(CommandType commandType) {
        switch (commandType) {
            case BACK -> {
                return Config.BACK_COMMAND_NAME.get();
            }
            case ACCEPT -> {
                return Config.TPAACCEPT_COMMAND_NAME.get();
            }
            case DENY -> {
                return Config.TPADENY_COMMAND_NAME.get();
            }
            case CANCEL -> {
                return Config.TPACANCEL_COMMAND_NAME.get();
            }
            case BLOCK -> {
                return Config.TPBLOCK_COMMAND_NAME.get();
            }
            case TOGGLE -> {
                return Config.TPTOGGLE_COMMAND_NAME.get();
            }
            case UNBLOCK -> {
                return Config.TPUNBLOCK_COMMAND_NAME.get();
            }
        }
        LOGGER.error(SWITCH_COMMAND_NAME_FAILURE_ERROR_MESSAGE);
        throw new IllegalStateException(SWITCH_COMMAND_NAME_FAILURE_ERROR_MESSAGE);
    }
}
