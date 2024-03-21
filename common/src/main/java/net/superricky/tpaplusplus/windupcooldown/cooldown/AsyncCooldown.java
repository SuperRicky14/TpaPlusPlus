package net.superricky.tpaplusplus.windupcooldown.cooldown;

import net.superricky.tpaplusplus.TPAPlusPlus;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.config.formatters.MessageParser;
import net.superricky.tpaplusplus.windupcooldown.windup.AsyncWindupHelper;
import net.superricky.tpaplusplus.windupcooldown.windup.WindupWatcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

public class AsyncCooldown {
    private static final Logger LOGGER = LoggerFactory.getLogger(TPAPlusPlus.MOD_ID);
    private static ScheduledExecutorService scheduler = null;// Executors.unconfigurableScheduledExecutorService(Executors.newScheduledThreadPool(1));

    private static List<CooldownData> queriedRemoveList = new ArrayList<>();

    public static void instantiateCooldownLoop() {
        LOGGER.info("Instantiating asynchronous cooldown loop...");
        scheduler.scheduleAtFixedRate(AsyncCooldown::runCooldown, 1, 1, TimeUnit.SECONDS);
    }

    public static void runCooldown() {
        for (CooldownData cooldownData : AsyncCooldownHelper.getCooldownSet()) {
            try {
                if (cooldownData.getDelay() < 0) {
                    // Delay is LOWER than OR EQUAL to zero, throw an error
                    throw new IllegalArgumentException("Delay for Scheduled Task must be greater than 0! Please report this issue to the TPA++ issue page immediately.");
                }

                if (Objects.isNull(cooldownData.getAffectedPlayer())) {
                    // Delay is LOWER than OR EQUAL to zero, throw an error
                    throw new IllegalArgumentException("Affected Player UUID is null! Please report this issue to the TPA++ issue page immediately.");
                }

                if (cooldownData.getDelay() > 0) {
                    // Delay is ABOVE zero, countdown not finished
                    if (Objects.isNull(scheduler)) {
                        LOGGER.error("IllegalStateException: Scheduler is null!");
                        throw new IllegalStateException("Scheduler is null!");
                    }

                    LOGGER.info(String.format("Decrementing delay from %s by 1...", cooldownData.getDelay()));
                    cooldownData.setDelay(cooldownData.getDelay() - 1);
                }
            } catch (IllegalArgumentException | IllegalStateException e) {
                LOGGER.error(e.getMessage());
            }

            // Delay is zero, countdown finished. Here we add it to our list of things to remove (we would get unpredictable behaviour if we removed it within this loop, then we just iterate and delete it later.
            queriedRemoveList.add(cooldownData);
        }

        for (CooldownData cooldownData : queriedRemoveList) {
            AsyncCooldownHelper.getCooldownSet().remove(cooldownData);
        }

        queriedRemoveList.clear();
    }

    private AsyncCooldown() {
    }
}
