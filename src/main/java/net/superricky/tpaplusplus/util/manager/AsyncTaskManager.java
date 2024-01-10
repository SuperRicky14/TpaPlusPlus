package net.superricky.tpaplusplus.util.manager;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.mojang.logging.LogUtils;
import net.minecraft.network.chat.Component;
import net.minecraftforge.common.MinecraftForge;
import net.superricky.tpaplusplus.Main;
import net.superricky.tpaplusplus.util.Request;
import net.superricky.tpaplusplus.util.configuration.Config;
import net.superricky.tpaplusplus.util.configuration.Messages;
import net.superricky.tpaplusplus.event.RequestAcceptSuccessEvent;
import net.superricky.tpaplusplus.event.RequestTimeoutEvent;
import net.superricky.tpaplusplus.util.configuration.formatters.MessageParser;
import net.superricky.tpaplusplus.util.limitations.LimitationManager;
import net.superricky.tpaplusplus.util.manager.saved.SaveDataManager;
import org.slf4j.Logger;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;


public class AsyncTaskManager {
    private static final Logger LOGGER = LogUtils.getLogger();
    private static final ScheduledExecutorService executorService = Executors.newScheduledThreadPool(Config.ASYNC_GENERAL_TASKS_THREAD_POOL.get());

    private AsyncTaskManager() {
    }

    public static void scheduleTeleportTimeout(Request request) {
        executorService.schedule(() ->
                        MinecraftForge.EVENT_BUS.post(new RequestTimeoutEvent(request)),
                Config.TPA_TIMEOUT_IN_SECONDS.get(), TimeUnit.SECONDS);
    }

    // Starts a countdown based on the configured time in the configTPAAcceptSuccessEvent
    public static synchronized void startTPAAcceptCountdown(Request request) {
        if (request.isAccepted()) {
            request.getReceiver().sendSystemMessage(Component.literal("§cYou have already accepted the request!"));
            return;
        }

        if (!(RequestManager.alreadySentTeleportRequest(request))) return;

        if (!LimitationManager.notifyAndCheckAllowedToTeleport(request.getReceiver(), request.getSender(), true)) return;

        request.setAccepted(true);

        if (request.isHereRequest()) request.setAcceptPosition(request.getReceiver().getX(), request.getReceiver().getY(), request.getReceiver().getZ());
        else request.setAcceptPosition(request.getSender().getX(), request.getSender().getY(), request.getSender().getZ());

        request.getReceiver().sendSystemMessage(Component.literal(String.format(Messages.ACCEPT_COUNTDOWN_MESSAGE_START.get(), Config.TPA_ACCEPT_TIME_IN_SECONDS.get())));

        executorService.schedule(() ->
                runTPAAcceptCountdown(request, Config.TPA_ACCEPT_TIME_IN_SECONDS.get()), 0, TimeUnit.SECONDS);
    }

    private static synchronized void runTPAAcceptCountdown(Request request, int timeoutInSeconds) {
        if (!RequestManager.alreadySentTeleportRequest(request)) return;

        if (!LimitationManager.notifyAndCheckAllowedToTeleport(request.getReceiver(), request.getSender(), true)) return;

        if (timeoutInSeconds > 0) {
            if (request.isHereRequest()) {
                request.getReceiver().sendSystemMessage(Component.literal(String.format(Messages.ACCEPT_COUNTDOWN_MESSAGE.get(), timeoutInSeconds)));

                if (Boolean.TRUE.equals(Config.SEND_TELEPORT_REQUEST_COUNTDOWN_TO_BOTH_PLAYERS.get()))
                    request.getSender().sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.OTHER_PLAYER_ACCEPT_COUNTDOWN_MESSAGE.get(),
                            Map.of("otherPlayerName", request.getReceiver().getName().getString(), "timeRemaining", timeoutInSeconds))));
            } else {
                request.getSender().sendSystemMessage(Component.literal(String.format(Messages.ACCEPT_COUNTDOWN_MESSAGE.get(), timeoutInSeconds)));

                if (Boolean.TRUE.equals(Config.SEND_TELEPORT_REQUEST_COUNTDOWN_TO_BOTH_PLAYERS.get()))
                    request.getReceiver().sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.OTHER_PLAYER_ACCEPT_COUNTDOWN_MESSAGE.get(),
                            Map.of("otherPlayerName", request.getSender().getName().getString(), "timeRemaining", timeoutInSeconds))));
            }

            executorService.schedule(() ->
                    runTPAAcceptCountdown(request, timeoutInSeconds - 1), 1, TimeUnit.SECONDS);
        } else {
            executorService.schedule(() ->
                    MinecraftForge.EVENT_BUS.post(new RequestAcceptSuccessEvent(request)), 0, TimeUnit.SECONDS);
        }
    }

    public static class AsyncAutosave {
        public static void initialiseAutoSaveService() {
            executorService.schedule(() ->
                    autoSave(), Config.AUTOSAVE_INTERVAL.get(), TimeUnit.SECONDS);
        }

        // Use recursion to continously AutoSave the PlayerData.
        private static void autoSave() {
            SaveDataManager.savePlayerData();

            executorService.schedule(() ->
                    autoSave(), Config.AUTOSAVE_INTERVAL.get(), TimeUnit.SECONDS);
        }
    }
}