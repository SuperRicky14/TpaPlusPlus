package net.superricky.tpaplusplus.windup;

import net.minecraft.network.chat.Component;
import net.superricky.tpaplusplus.TPAPlusPlus;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.config.formatters.MessageParser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import static net.superricky.tpaplusplus.TPAPlusPlus.distance3D;

public class WindupWatcher {
    private static ScheduledExecutorService scheduler = Executors.unconfigurableScheduledExecutorService(Executors.newScheduledThreadPool(1));
    private static final Set<WindupData> trackedWindupData = ConcurrentHashMap.newKeySet();

    private static final Logger WATCHDOG_LOGGER = LoggerFactory.getLogger(TPAPlusPlus.MOD_ID + "_WATCHDOG");
    private static final Logger LOGGER = LoggerFactory.getLogger(TPAPlusPlus.MOD_ID);

    private static final String SWITCH_DISTANCE_FAILURE_ERROR_MESSAGE = "Switch statement could not find the respective windup distance!";
    private static final String SWITCH_COMMAND_NAME_FAILURE_ERROR_MESSAGE = "Switch statement could not find the respective command!";

    private static final boolean USE_NON_BLOCKING_ASYNC_TICK_LOOP = Config.USE_NON_BLOCKING_ASYNC_TICK_LOOP.get();

    public static void clearTrackedWindupData() {
        trackedWindupData.clear();
    }

    public static Set<WindupData> getTrackedWindupData() {
        return trackedWindupData;
    }

    public static void watchWindupDataPosition() {
        runTick(); // Non-Blocking Tick Loop is disabled, run it synchronously with the main thread.
    }

    public static void startAsyncTickLoop(int rate) {
        scheduler.scheduleAtFixedRate(() -> {
            runTick();

            // TODO: Implement a warning logging system for when this tick loop lags out.
        }, 1000 / rate, 1000 / rate, TimeUnit.MILLISECONDS);
    }

    private static void runTick() {
        for (WindupData windupData : trackedWindupData) {
            double windupDistance = getWindupDistance(windupData);

            if (windupDistance == -1) continue;

            if (windupData.getCancelled().get()) continue;

            if (distance3D(
                    windupData.getAcceptX(),
                    windupData.getAcceptY(),
                    windupData.getAcceptZ(),
                    windupData.getPlayers()[0].getX(),
                    windupData.getPlayers()[0].getY(),
                    windupData.getPlayers()[0].getZ()
            ) > windupDistance) {
                // Distance between the position which they started the countdown, and their current position is larger than the allowed distance set in the Config.
                windupData.getCancelled().set(true);

                windupData.getPlayers()[0].sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.PLAYER_MOVED_DURING_WINDUP.get(), Map.of("command_used", getWindupCommand(windupData)))));

                trackedWindupData.remove(windupData);
            }
        }
    }

    private static double getWindupDistance(WindupData windupData) {
        switch (windupData.getType()) {
            case BACK -> {
                return Config.BACK_WINDUP_DISTANCE.get();
            }
            case ACCEPT -> {
                return Config.ACCEPT_WINDUP_DISTANCE.get();
            }
            case DENY -> {
                return Config.DENY_WINDUP_DISTANCE.get();
            }
            case CANCEL -> {
                return Config.CANCEL_WINDUP_DISTANCE.get();
            }
            case SEND -> {
                return Config.SEND_WINDUP_DISTANCE.get();
            }
            case BLOCK -> {
                return Config.BLOCK_WINDUP_DISTANCE.get();
            }
            case TOGGLE -> {
                return Config.TOGGLE_WINDUP_DISTANCE.get();
            }
            case UNBLOCK -> {
                return Config.UNBLOCK_WINDUP_DISTANCE.get();
            }
        }
        LOGGER.error(SWITCH_DISTANCE_FAILURE_ERROR_MESSAGE);
        throw new IllegalStateException(SWITCH_DISTANCE_FAILURE_ERROR_MESSAGE);
    }

    private static String getWindupCommand(WindupData windupData) {
        switch (windupData.getType()) {
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
                if (Boolean.TRUE.equals(windupData.getHereRequest())) {
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

    public static boolean deInstantiateScheduledExecutorService() throws InterruptedException {
        if (USE_NON_BLOCKING_ASYNC_TICK_LOOP) {
            LOGGER.warn("A call to de-instantiate the ScheduledExecutorService for the non-blocking async tick loop, was called EVEN THOUGH the non-blocking tick loop is ENABLED!");
            LOGGER.warn("This call will be ignored, it is safe to continue playing but consider reporting this issue to TPA++");
            return true;
        }

        if (scheduler.isShutdown())
            throw new IllegalStateException("Attempted to shutdown the ScheduledExecutorService but it was already shutdown beforehand!");

        // Shutdown the ScheduledExecutorService immediately
        scheduler.shutdownNow();

        // Forcefully shutdown the executor
        return scheduler.awaitTermination(5, TimeUnit.SECONDS);
    }

    public static boolean stopScheduledExecutorService() throws InterruptedException {
        if (USE_NON_BLOCKING_ASYNC_TICK_LOOP) {
            if (scheduler.isShutdown())
                throw new IllegalStateException("Attempted to shutdown the ScheduledExecutorService but it was already shutdown beforehand!");

            WATCHDOG_LOGGER.warn("NON-BLOCKING TICK LOOP SHUTTING DOWN..."); // Only warn when non-blocking async tick loop is enabled

            // Shutdown the ScheduledExecutorService immediately
            scheduler.shutdownNow();

            // Forcefully shutdown the executor
            return scheduler.awaitTermination(5, TimeUnit.SECONDS);
        }

        return false;
    }

    public static void reCreateScheduledExecutorService() {
        if (USE_NON_BLOCKING_ASYNC_TICK_LOOP) {
            if (Boolean.FALSE.equals(scheduler.isShutdown())) {
                throw new IllegalStateException("Attempted to re-create ScheduledExecutorService but it was not shutdown beforehand!");
            }
            // Create a new ScheduledExecutorService
            scheduler = Executors.unconfigurableScheduledExecutorService(Executors.newScheduledThreadPool(1));

            WATCHDOG_LOGGER.info("NON-BLOCKING TICK LOOP RESTARTING...");

            // Restart the Tick Loop
            startAsyncTickLoop(Config.ASYNC_TICK_LOOP_UPDATE_RATE.get());
        }
    }

    private WindupWatcher() {
    }
}
