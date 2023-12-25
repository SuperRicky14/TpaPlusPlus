package net.superricky.tpaplusplus;

import net.minecraft.network.chat.Component;
import net.minecraftforge.common.MinecraftForge;
import net.superricky.tpaplusplus.event.TPAAcceptSuccessEvent;
import net.superricky.tpaplusplus.event.TeleportRequestTimeoutEvent;
import net.superricky.tpaplusplus.teleport.Teleport;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

public class TeleportScheduler {
    private TeleportScheduler() {}

    private static ScheduledExecutorService executorService = Executors.newScheduledThreadPool(1);

    public static synchronized void scheduleTeleportTimeout(Teleport teleportRequest, int timeoutInSeconds) {
        executorService.schedule(() -> {
            MinecraftForge.EVENT_BUS.post(new TeleportRequestTimeoutEvent(teleportRequest));

            // Remove the entry
            Main.teleportRequests.remove(teleportRequest);
        }, timeoutInSeconds, TimeUnit.SECONDS);
    }

    public static synchronized void startTPAAcceptCountdown(Teleport teleportRequest, int timeoutInSeconds) {
        if (!(Main.teleportRequests.containsKey(teleportRequest))) return;

        if (timeoutInSeconds > 0) {
            teleportRequest.teleported().sendSystemMessage(
                    Component.literal("§6" + timeoutInSeconds));

            executorService.schedule(() ->
                    startTPAAcceptCountdown(teleportRequest, timeoutInSeconds - 1), 1, TimeUnit.SECONDS);
        } else {
            executorService.schedule(() ->
            {
                MinecraftForge.EVENT_BUS.post(
                        new TPAAcceptSuccessEvent(teleportRequest));

                Main.teleportRequests.remove(teleportRequest);
            }, 0, TimeUnit.SECONDS);
        }
    }
}
