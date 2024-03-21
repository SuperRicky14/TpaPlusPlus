package net.superricky.tpaplusplus.timeout;

import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.requests.Request;

import java.util.Objects;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

public class TimeoutScheduler {
    // Create a shared thread pool
    private static ScheduledExecutorService scheduler = Executors.unconfigurableScheduledExecutorService(Executors.newScheduledThreadPool(1));

    public static void scheduleTeleportTimeout(Request request) {
        scheduler.schedule(() ->
                        RequestTimeoutEvent.EVENT.invoker().onRequestTimeout(request),
                Config.TPA_TIMEOUT_IN_SECONDS.get(), TimeUnit.SECONDS);
    }

    public static boolean stopScheduledExecutorService() throws InterruptedException {
        if (Objects.isNull(scheduler) || scheduler.isShutdown())
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

    public static boolean forceQuitScheduledExecutorService() throws InterruptedException {
        // Shutdown the ScheduledExecutorService immediately
        scheduler.shutdownNow();

        // Forcefully shutdown the executor
        return scheduler.awaitTermination(5, TimeUnit.SECONDS);
    }

    private TimeoutScheduler() {
    }
}
