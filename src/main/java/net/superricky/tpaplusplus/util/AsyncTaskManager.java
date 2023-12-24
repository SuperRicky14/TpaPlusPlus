package net.superricky.tpaplusplus.util;

import com.mojang.logging.LogUtils;
import net.minecraftforge.common.MinecraftForge;
import net.superricky.tpaplusplus.Config;
import net.superricky.tpaplusplus.event.RequestTimeoutEvent;
import org.apache.commons.lang3.NotImplementedException;
import org.slf4j.Logger;

import java.util.concurrent.*;

public class AsyncTaskManager {
    private AsyncTaskManager() {}
    private static final Logger LOGGER = LogUtils.getLogger();

    private static ScheduledExecutorService executorService = Executors.newScheduledThreadPool(1);


    public static synchronized void scheduleTeleportTimeout(Request request) {
        executorService.schedule(() -> {
            MinecraftForge.EVENT_BUS.post(new RequestTimeoutEvent(request));
        }, Config.TPA_TIMEOUT_IN_SECONDS.get(), TimeUnit.SECONDS);
    }

    // A function that utilises recursion to automatically count down until 0, then it will automatically post a TPAAcceptSuccessEvent
    public static synchronized void startTPAAcceptCountdown(Request request, int timeoutInSeconds) {
        throw new NotImplementedException("startTPAAcceptCountdown in AsyncTaskManager dosen't yet have functionality!");
        /*if (!(TeleportHelper.alreadySentTeleportRequest(teleportRequest))) return;

        if (timeoutInSeconds > 0) {
            if (teleportRequest instanceof TeleportHere) {
                teleportRequest.teleported().sendSystemMessage(Component.literal("§6" + timeoutInSeconds));
            } else if (teleportRequest instanceof TeleportTo) {
                teleportRequest.executor().sendSystemMessage(Component.literal("§6" + timeoutInSeconds));
            }
            executorService.schedule(() ->
                    startTPAAcceptCountdown(teleportRequest, timeoutInSeconds - 1), 1, TimeUnit.SECONDS);
        } else {
            executorService.schedule(() -> {
                MinecraftForge.EVENT_BUS.post(
                        new TPAAcceptSuccessEvent(teleportRequest));

                Main.teleportRequests.remove(teleportRequest);
            }, 0, TimeUnit.SECONDS);*/
        }
}
