package net.superricky.tpaplusplus.commands.unblock;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.commands.block.PlayerBlockingHelper;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.config.formatters.MessageParser;
import net.superricky.tpaplusplus.io.PlayerData;
import net.superricky.tpaplusplus.io.SaveDataManager;
import net.superricky.tpaplusplus.requests.RequestHelper;
import net.superricky.tpaplusplus.windupcooldown.CommandType;
import net.superricky.tpaplusplus.windupcooldown.windup.AsyncWindup;
import net.superricky.tpaplusplus.windupcooldown.windup.WindupData;

import java.util.Map;

public class UnBlockPlayer {
    public static void unBlockPlayer(ServerPlayer executor, ServerPlayer blockedPlayer) {
        if (RequestHelper.isPlayerIdentical(executor, blockedPlayer)) {
            // Player is trying to block themselves
            executor.sendSystemMessage(Component.literal(Messages.CANNOT_UNBLOCK_SELF.get()));
            return;
        }

        PlayerData executorData = SaveDataManager.getPlayerData(executor);

        if (Boolean.FALSE.equals(executorData.getBlockedPlayers().contains(blockedPlayer.getUUID()))) {
            // Executor has not blocked the other player
            executor.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.HAVENT_BLOCKED_PLAYER.get(),
                    Map.of(PlayerBlockingHelper.BLOCKED_PLAYER_CONSTANT, blockedPlayer.getName().getString()))));
            return;
        }

        if (Config.UNBLOCK_WINDUP.get() == 0) {
            absoluteUnBlockPlayer(executorData, executor, blockedPlayer);
        } else {
            AsyncWindup.schedule(new WindupData(executorData, Config.UNBLOCK_WINDUP.get(), executor.getX(), executor.getY(), executor.getZ(), CommandType.UNBLOCK, new ServerPlayer[]{executor, blockedPlayer}));
        }
    }

    public static void absoluteUnBlockPlayer(PlayerData executorData, ServerPlayer executor, ServerPlayer blockedPlayer) {
        executorData.getBlockedPlayers().remove(blockedPlayer.getUUID()); // Remove the blocked player from the executors list.

        executor.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.SENDER_UNBLOCKED_PLAYER.get(),
                Map.of("unblocked_player", blockedPlayer.getName().getString()))));

        if (Boolean.TRUE.equals(Config.SEND_BLOCKED_MESSAGES_TO_BOTH_PLAYERS.get())) {
            // Sending Blocked / Unblocked Messages has been enabled in the config
            blockedPlayer.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.PLAYER_UNBLOCKED_BY_SENDER.get(),
                    Map.of("sender_name", executor.getName().getString()))));
        }
    }

    private UnBlockPlayer() {
    }
}
