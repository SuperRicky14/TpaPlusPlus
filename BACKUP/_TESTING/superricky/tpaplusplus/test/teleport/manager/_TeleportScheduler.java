
package _TESTING.superricky.tpaplusplus.test.teleport.manager;

import _TESTING.superricky.tpaplusplus.test.event._TeleportRequestTimeoutEvent;
import net.minecraft.network.chat.Component;
import net.minecraftforge.common.MinecraftForge;
import _TESTING.superricky.tpaplusplus.test._Main;
import _TESTING.superricky.tpaplusplus.test.event._TPAAcceptSuccessEvent;
import _TESTING.superricky.tpaplusplus.test.teleport._Teleport;
import _TESTING.superricky.tpaplusplus.test.teleport._TeleportHere;
import _TESTING.superricky.tpaplusplus.test.teleport._TeleportTo;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

public class _TeleportScheduler {
    private _TeleportScheduler() {}

    private static ScheduledExecutorService executorService = Executors.newScheduledThreadPool(1);

    public static synchronized void scheduleTeleportTimeout(_Teleport teleportRequest, int timeoutInSeconds) {
        executorService.schedule(() -> {
            MinecraftForge.EVENT_BUS.post(new _TeleportRequestTimeoutEvent(teleportRequest));

            // Remove the entry
            _Main.TELEPORT_REQUESTS.remove(teleportRequest);
        }, timeoutInSeconds, TimeUnit.SECONDS);
    }

    // A function that utilises recursion to automatically count down until 0, then it will automatically post a TPAAcceptSuccessEvent
    public static synchronized void startTPAAcceptCountdown(_Teleport teleportRequest, int timeoutInSeconds) {
        if (!(_TeleportHelper.alreadySentTeleportRequest(teleportRequest))) return;

        if (timeoutInSeconds > 0) {
            if (teleportRequest instanceof _TeleportHere) {
                teleportRequest.teleported().sendSystemMessage(Component.literal("§6" + timeoutInSeconds));
            } else if (teleportRequest instanceof _TeleportTo) {
                teleportRequest.executor().sendSystemMessage(Component.literal("§6" + timeoutInSeconds));
            }
            executorService.schedule(() ->
                    startTPAAcceptCountdown(teleportRequest, timeoutInSeconds - 1), 1, TimeUnit.SECONDS);
        } else {
            executorService.schedule(() -> {
                MinecraftForge.EVENT_BUS.post(
                        new _TPAAcceptSuccessEvent(teleportRequest));

                _Main.TELEPORT_REQUESTS.remove(teleportRequest);
            }, 0, TimeUnit.SECONDS);
        }
    }
}