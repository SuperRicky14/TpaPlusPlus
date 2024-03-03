package net.superricky.tpaplusplus.windupcooldown.windup;

import net.minecraft.network.chat.Component;
import net.superricky.tpaplusplus.TPAPlusPlus;
import net.superricky.tpaplusplus.commands.accept.AcceptTPA;
import net.superricky.tpaplusplus.commands.back.Back;
import net.superricky.tpaplusplus.commands.block.BlockPlayer;
import net.superricky.tpaplusplus.commands.cancel.CancelTPA;
import net.superricky.tpaplusplus.commands.deny.DenyTPA;
import net.superricky.tpaplusplus.commands.send.SendTPA;
import net.superricky.tpaplusplus.commands.toggle.TPToggle;
import net.superricky.tpaplusplus.commands.unblock.UnBlockPlayer;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.config.formatters.MessageParser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

public class AsyncWindup {
    // We have to create our own executor to allow us to modify the internal scheduler.
    private static ScheduledExecutorService scheduler = Executors.unconfigurableScheduledExecutorService(Executors.newScheduledThreadPool(1));

    private static final Logger LOGGER = LoggerFactory.getLogger(TPAPlusPlus.MOD_ID);

    public static void schedule(WindupData windupData) {
        if (Objects.isNull(scheduler)) {
            LOGGER.error("IllegalStateException: Scheduler is null!");
            throw new IllegalStateException("Scheduler is null!");
        }

        if (windupData.getDelay() <= 0) {
            // Delay is LOWER than OR EQUAL to zero, throw an error
            LOGGER.error("IllegalArgumentException: Delay for Scheduled Task must be greater than 0! Please report this issue to the TPA++ issue page immediately.");
            AsyncWindupHelper.fastMSG("Delay for Scheduled Task must be greater than 0! Please report this issue to the TPA++ issue page immediately.", windupData.getPlayers());
            throw new IllegalArgumentException("Delay for Scheduled Task must be greater than 0! Please report this issue to the TPA++ issue page immediately.");
        }

        if (AsyncWindupHelper.playersAreNull(windupData.getPlayers())) {
            LOGGER.error("IllegalArgumentException: The playerlist or one of the players inside is null! Please report this issue to the TPA++ issue page immediately.");
            throw new IllegalArgumentException("The playerlist or one of the players inside is null! Please report this issue to the TPA++ issue page immediately.");
        }

        if (windupData.getPlayers().length == 0) {
            LOGGER.error("IllegalArgumentException: The playerlist or one of the players inside is null! Please report this issue to the TPA++ issue page immediately.");
            AsyncWindupHelper.fastMSG("No players were specified when attempting to schedule this task! Please report this issue to the TPA++ issue page immediately.", windupData.getPlayers());
            throw new IllegalArgumentException("No players were specified when attempting to schedule this task! Please report this issue to the TPA++ issue page immediately.");
        }

        // Prevent the tasks specified from being in an illegal state (e.g: the caller specified an ACCEPT type, although there was no request to accept).
        try {
            AsyncWindupHelper.getErrorMessage(windupData);
        } catch (IllegalArgumentException e) {
            LOGGER.error(e.getMessage());
            windupData.getPlayers()[0].sendSystemMessage(Component.literal("§4A fatal internal server error occurred, please check console for more information."));
            return;
        }

        if (windupData.getCancelled().get()) {
            throw new IllegalArgumentException("Tried to schedule a windupData that has already been cancelled.");
        }

        if (Objects.isNull(scheduler)) {
            LOGGER.error("IllegalStateException: Scheduler is null!");
            throw new IllegalStateException("Scheduler is null!");
        }

        // Request is a valid request.
        WindupWatcher.getTrackedWindupData().add(windupData); // Add it to the tracked windupData.
        scheduler.execute(() -> countdown(windupData));
    }

    /**
     * A method which actually performs the countdown for any given task.
     * You might get a lot of warnings here about potential NullPointerException, but that should all be handled by the schedule method, which should prevent anything from being null in the first place.
     */
    private static void countdown(WindupData windupData) {
        if (windupData.getDelay() < 0) {
            // Delay is LOWER than zero, throw an error, also notify the player about unsafe usage of this method
            LOGGER.error("IllegalArgumentException: Delay for Scheduled Task must be greater than 0, but somehow this was not caught by the caller??");
            throw new IllegalArgumentException("Delay for Scheduled Task must be greater than 0, but somehow this was not caught by the caller??");
        }

        if (windupData.getDelay() > 0) {
            // Delay is ABOVE zero, countdown not finished
            if (Objects.isNull(scheduler)) {
                LOGGER.error("IllegalStateException: Scheduler is null!");
                throw new IllegalStateException("Scheduler is null!");
            }

            if (windupData.getCancelled().get()) {
                WindupWatcher.getTrackedWindupData().remove(windupData);
                return;
            }

            AsyncWindupHelper.fastMSG(MessageParser.enhancedFormatter(Messages.WINDUP_TIME_REMAINING.get(), Map.of("time", Integer.toString(windupData.getDelay()))), windupData.getPlayers());
            windupData.setDelay(windupData.getDelay() - 1);
            scheduler.schedule(() -> countdown(windupData), 1, TimeUnit.SECONDS);
            return;
        }

        if (windupData.getCancelled().get()) {
            WindupWatcher.getTrackedWindupData().remove(windupData);
            return;
        }

        // Delay is zero, countdown finished
        try {
            switch (windupData.getType()) {
                // Run the /back method
                case BACK ->
                        Back.absoluteTeleportToLatestDeath(windupData.getPlayers()[0], windupData.getDeathPosition());

                // Run the /tpaaccept method
                case ACCEPT ->
                        AcceptTPA.absoluteAcceptFunctionality(Objects.requireNonNull(windupData.getRequest()), windupData.getPlayers()[1]);

                // Run the /tpadeny method
                case DENY -> DenyTPA.absoluteDeny(Objects.requireNonNull(windupData.getRequest()));

                // Run the /tpacancel method
                case CANCEL -> CancelTPA.absoluteCancel(Objects.requireNonNull(windupData.getRequest()));

                // Run the TPA method
                case SEND ->
                        SendTPA.absoluteSendTeleportRequest(windupData.getPlayers()[0], windupData.getPlayers()[1], Objects.requireNonNull(windupData.getHereRequest()));

                // Run the /tpblock method
                case BLOCK ->
                        BlockPlayer.absoluteBlockPlayer(Objects.requireNonNull(windupData.getPlayerData()), windupData.getPlayers()[0], windupData.getPlayers()[1]);

                // Run the /tptoggle method
                case TOGGLE -> TPToggle.toggleTP(windupData.getPlayers()[0]);

                // Run the /tpunblock method
                case UNBLOCK ->
                        UnBlockPlayer.absoluteUnBlockPlayer(Objects.requireNonNull(windupData.getPlayerData()), windupData.getPlayers()[0], windupData.getPlayers()[1]);
            }
        } catch (NullPointerException e) {
            LOGGER.error(e.getMessage());
            LOGGER.warn("A NullPointerException was caught by TPA++. This is an extremely rare case, and if this does not happen again you should be able to continue playing.");
            LOGGER.warn("Although, please report this issue to TPA++ at \"https://github.com/SuperRicky14/TpaPlusPlus/issues\".");
            LOGGER.warn("If you notice any unexpected behaviour shutdown your server immediately and create a backup of your world before turning it back on.");
        }

        WindupWatcher.getTrackedWindupData().remove(windupData);
    }

    public static boolean stopScheduledExecutorService() throws InterruptedException {
        if (scheduler.isShutdown())
            throw new IllegalStateException("Attempted to shutdown the ScheduledExecutorService but it was already shutdown beforehand!");

        // Shutdown the ScheduledExecutorService immediately
        scheduler.shutdownNow();

        // Forcefully shutdown the executor
        return scheduler.awaitTermination(5, TimeUnit.SECONDS);
    }

    public static void reCreateScheduledExecutorService() {
        if (Boolean.FALSE.equals(scheduler.isShutdown()))
            throw new IllegalStateException("Attempted to re-create ScheduledExecutorService but it was not shutdown beforehand!");

        // Create a new ScheduledExecutorService
        scheduler = Executors.unconfigurableScheduledExecutorService(Executors.newScheduledThreadPool(1));
    }

    private AsyncWindup() {
    }
}

