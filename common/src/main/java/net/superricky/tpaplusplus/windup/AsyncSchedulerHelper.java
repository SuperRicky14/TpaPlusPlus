package net.superricky.tpaplusplus.windup;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;

import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

public class AsyncSchedulerHelper {

    /**
     * A method for checking whether a method using the schedule function actually passed in a proper set of players.
     * @return true if the list of players is null, false if it isn't
     */
    static boolean playersAreNull(ServerPlayer... players) {
        if (Objects.isNull(players)) return true;

        for (ServerPlayer player : players)
            if (Objects.isNull(player)) return true;

        return false;
    }

    static void fastMSG(String message, ServerPlayer... players) {
        for (ServerPlayer player : players)
            player.sendSystemMessage(Component.literal(message));
    }

    static void getErrorMessage(WindupData windupData) {
        switch (windupData.getType()) {
            case BACK -> {
                if (Objects.isNull(windupData.getDeathPosition())) {
                    throw new IllegalArgumentException("A TPA++ BACK task was scheduled, although there was no LevelBoundVec3 to accompany it?? Please report this issue to the TPA++ issue page immediately.");
                }
            }
            case ACCEPT -> {
                if (Objects.isNull(windupData.getRequest())) {
                    throw new IllegalArgumentException("A TPA++ ACCEPT task was scheduled, although there was no Request to accompany it?? Please report this issue to the TPA++ issue page immediately.");
                }
            }
            case CANCEL -> {
                if (Objects.isNull(windupData.getRequest())) {
                    throw new IllegalArgumentException("A TPA++ CANCEL task was scheduled, although there was no Request to accompany it?? Please report this issue to the TPA++ issue page immediately.");
                }
            }
            case SEND -> {
                if (Objects.isNull(windupData.getHereRequest())) {
                    throw new IllegalArgumentException("A TPA++ SEND task was scheduled, although there was no boolean \"isHereRequest\" to accompany it?? Please report this issue to the TPA++ issue page immediately.");
                }
            }
            case BLOCK -> {
                if (Objects.isNull(windupData.getPlayerData())) {
                    throw new IllegalArgumentException("A TPA++ BLOCK task was scheduled, although there was no PlayerData to accompany it?? Please report this issue to the TPA++ issue page immediately.");
                }
            }
            case UNBLOCK -> {
                if (Objects.isNull(windupData.getPlayerData())) {
                    throw new IllegalArgumentException("A TPA++ UNBLOCK task was scheduled, although there was no PlayerData to accompany it?? Please report this issue to the TPA++ issue page immediately.");
                }
            }
        }
    }

    private AsyncSchedulerHelper() {
    }
}
