package net.superricky.tpaplusplus;

import dev.architectury.event.events.common.CommandRegistrationEvent;
import dev.architectury.event.events.common.EntityEvent;
import dev.architectury.event.events.common.LifecycleEvent;
import dev.architectury.event.events.common.TickEvent;
import net.superricky.tpaplusplus.command.*;
import net.superricky.tpaplusplus.event.RequestAcceptSuccessEvent;
import net.superricky.tpaplusplus.event.RequestMovedEvent;
import net.superricky.tpaplusplus.event.RequestTimeoutEvent;
import net.superricky.tpaplusplus.util.manager.EventHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TPAPlusPlus {
    public static final String MOD_ID = "tpaplusplus";
    public static final String MOD_VERSION = "1.0.0";
    private static final Logger LOGGER = LoggerFactory.getLogger(MOD_ID);
    // We can use this if we don't want to use DeferredRegister

    public static final String CONFIG_PATH = "tpaplusplus-config.toml";
    public static final String MESSAGES_CONFIG_PATH = "tpaplusplus-messages.toml";

    public static void init() {
        LOGGER.info("INITIALIZING...");

        CommandRegistrationEvent.EVENT.register(BackCommand::onRegisterCommandEvent);
        CommandRegistrationEvent.EVENT.register(TPAAcceptCommand::onRegisterCommandEvent);
        CommandRegistrationEvent.EVENT.register(TPACancelCommand::onRegisterCommandEvent);
        CommandRegistrationEvent.EVENT.register(TPACommand::onRegisterCommandEvent);
        CommandRegistrationEvent.EVENT.register(TPADenyCommand::onRegisterCommandEvent);
        CommandRegistrationEvent.EVENT.register(TPAHereCommand::onRegisterCommandEvent);
        CommandRegistrationEvent.EVENT.register(TPAPlusPlusCommand::onRegisterCommandEvent);
        CommandRegistrationEvent.EVENT.register(TPBlockCommand::onRegisterCommandEvent);
        CommandRegistrationEvent.EVENT.register(TPToggleCommand::onRegisterCommandEvent);
        CommandRegistrationEvent.EVENT.register(TPUnBlockCommand::onRegisterCommandEvent);

        LifecycleEvent.SERVER_STARTED.register(state -> EventHandler.onServerStart());
        LifecycleEvent.SERVER_STOPPING.register(state -> EventHandler.onServerStop());
        TickEvent.SERVER_PRE.register(EventHandler::measurePlayerDistanceOverAccept);
        EntityEvent.LIVING_DEATH.register(EventHandler::onDeath);

        RequestMovedEvent.EVENT.register(EventHandler::onTPAMove);
        RequestAcceptSuccessEvent.EVENT.register(EventHandler::onTPAAcceptTimerSuccess);
        RequestTimeoutEvent.EVENT.register(EventHandler::onTimeoutEvent);

        LOGGER.info("...INITIALIZATION COMPLETE");
    }



    public static double euclideanDistanceCalculator3D(double x1, double y1, double z1, double x2, double y2, double z2) {
        return Math.sqrt(Math.pow(x2 - x1, 2) +
                Math.pow(y2 - y1, 2) +
                Math.pow(z2 - z1, 2));
    }
}
