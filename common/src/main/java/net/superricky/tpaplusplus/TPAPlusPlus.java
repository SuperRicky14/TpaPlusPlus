package net.superricky.tpaplusplus;

import dev.architectury.event.events.common.CommandRegistrationEvent;
import dev.architectury.event.events.common.EntityEvent;
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
import net.superricky.tpaplusplus.io.ServerLifecycleHandler;
import net.superricky.tpaplusplus.network.UpdateCheckKt;
import net.superricky.tpaplusplus.timeout.RequestTimeoutEvent;
import net.superricky.tpaplusplus.timeout.TimeoutManagerKt;
import net.superricky.tpaplusplus.util.MsgFmt;
import net.superricky.tpaplusplus.windupcooldown.windup.WindupWatcherKt;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

public class TPAPlusPlus {
    public static final String MOD_ID = "tpaplusplus";
    public static final String MOD_VERSION = "1.6.0-1.20.x-RELEASE-CANDIDATE-1";
    private static final Logger LOGGER = LoggerFactory.getLogger(MOD_ID);

    public static final String CONFIG_PATH = "tpaplusplus-config.toml";
    public static final String MESSAGES_CONFIG_PATH = "tpaplusplus-messages.toml";

    public static void init() {
        LOGGER.info("INITIALIZING...");

        LOGGER.info("REGISTERING BACK COMMAND...");
        CommandRegistrationEvent.EVENT.register((dispatcher, registry, selection) -> BackCommand.onRegisterCommandEvent(dispatcher));
        LOGGER.info("REGISTERING TPAACCEPT COMMAND...");
        CommandRegistrationEvent.EVENT.register((dispatcher, registry, selection) -> TPAAcceptCommand.onRegisterCommandEvent(dispatcher));
        LOGGER.info("REGISTERING TPACANCEL COMMAND...");
        CommandRegistrationEvent.EVENT.register((dispatcher, registry, selection) -> TPACancelCommand.onRegisterCommandEvent(dispatcher));
        LOGGER.info("REGISTERING TPA COMMAND...");
        CommandRegistrationEvent.EVENT.register((dispatcher, registry, selection) -> TPACommand.onRegisterCommandEvent(dispatcher));
        LOGGER.info("REGISTERING TPADENY COMMAND...");
        CommandRegistrationEvent.EVENT.register((dispatcher, registry, selection) -> TPADenyCommand.onRegisterCommandEvent(dispatcher));
        LOGGER.info("REGISTERING TPAHERE COMMAND...");
        CommandRegistrationEvent.EVENT.register((dispatcher, registry, selection) -> TPAHereCommand.onRegisterCommandEvent(dispatcher));
        LOGGER.info("REGISTERING TPAPLUSPLUS COMMAND...");
        CommandRegistrationEvent.EVENT.register((dispatcher, registry, selection) -> TPAPlusPlusCommand.onRegisterCommandEvent(dispatcher));
        LOGGER.info("REGISTERING TPBLOCK COMMAND...");
        CommandRegistrationEvent.EVENT.register((dispatcher, registry, selection) -> TPBlockCommand.onRegisterCommandEvent(dispatcher));
        LOGGER.info("REGISTERING TPTOGGLE COMMAND...");
        CommandRegistrationEvent.EVENT.register((dispatcher, registry, selection) -> TPToggleCommand.onRegisterCommandEvent(dispatcher));
        LOGGER.info("REGISTERING TPUNBLOCK COMMAND...");
        CommandRegistrationEvent.EVENT.register((dispatcher, registry, selection) -> TPUnBlockCommand.onRegisterCommandEvent(dispatcher));

        LOGGER.info("REGISTERING \"LifecycleEvent.LIVING_DEATH\"...");
        EntityEvent.LIVING_DEATH.register((deadEntity, source) -> DeathHelper.onDeath(deadEntity));

        LOGGER.info("REGISTERING \"RequestTimeoutEvent\"...");
        RequestTimeoutEvent.EVENT.register(TimeoutManagerKt::onTimeoutEvent);

        if (Config.USE_NON_BLOCKING_ASYNC_TICK_LOOP.get()) {
            LOGGER.warn("USING EXPERIMENTAL NON BLOCKING TICK LOOP");
            LOGGER.info(MsgFmt.fmt("INITIALIZING TICK LOOP WITH RATE OF ${tick_rate}...", Map.of("tick_rate", Config.ASYNC_TICK_LOOP_UPDATE_RATE.get())));
            WindupWatcherKt.startAsyncTickLoop(Config.ASYNC_TICK_LOOP_UPDATE_RATE.get());
        } else {
            LOGGER.info("USING SYNCHRONOUS TICK LOOP");
            LOGGER.info("REGISTERING \"TickEvent.SERVER_POST\"...");
            TickEvent.SERVER_POST.register(server -> WindupWatcherKt.runTick());
        }

        LOGGER.info("INITIALIZING VERSION CHECKING...");
        UpdateCheckKt.initVersionCheckDaemon();

        LOGGER.info("FINAL SETUP...");
        ServerLifecycleHandler.onServerStart();

        LOGGER.info("INITIALIZING SHUTDOWN HOOK FOR CLEANUP...");
        Runtime.getRuntime().addShutdownHook(new Thread(ServerLifecycleHandler::onServerStop));

        LOGGER.info("...INITIALIZATION COMPLETE");
    }

    public static double distance3D(double x1, double y1, double z1, double x2, double y2, double z2) {
        double diffX = x2 - x1;
        double diffY = y2 - y1;
        double diffZ = z2 - z1;

        return Math.sqrt(diffX * diffX + diffY * diffY + diffZ * diffZ);
    }
}
