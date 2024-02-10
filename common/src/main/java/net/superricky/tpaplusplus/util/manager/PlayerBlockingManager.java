package net.superricky.tpaplusplus.util.manager;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.util.configuration.Config;
import net.superricky.tpaplusplus.util.configuration.Messages;
import net.superricky.tpaplusplus.util.configuration.formatters.MessageParser;
import net.superricky.tpaplusplus.util.manager.saved.PlayerData;
import net.superricky.tpaplusplus.util.manager.saved.SaveDataManager;

import java.util.Map;
import java.util.UUID;

public class PlayerBlockingManager {
    private static final String BLOCKED_PLAYER_CONSTANT = "blocked_player";

    public static boolean isPlayerBlocked(ServerPlayer sender, ServerPlayer receiver) {
        for (UUID senderBlockedUUID : SaveDataManager.getPlayerData(sender).getBlockedPlayers()) {
            if (receiver.getUUID().equals(senderBlockedUUID)) {
                // The sender blocked the receiver
                sender.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.SENDER_BLOCKED_RECEIVER.get(),
                        Map.of("blocked_player", receiver.getName().getString()))));
                return true;
            }
        }

        for (UUID receiverBlockedUUID : SaveDataManager.getPlayerData(receiver).getBlockedPlayers()) {
            if (sender.getUUID().equals(receiverBlockedUUID)) {
                // The receiver blocked the sender
                sender.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.RECEIVER_BLOCKED_SENDER.get(),
                        Map.of("blocking_player", receiver.getName().getString()))));
                return true;
            }
        }
        return false;
    }

    public static void blockPlayer(ServerPlayer executor, ServerPlayer blockedPlayer) {
        if (RequestManager.isPlayerIdentical(executor, blockedPlayer)) {
            // Player is trying to block themselves
            executor.sendSystemMessage(Component.literal(Messages.CANNOT_BLOCK_SELF.get()));
            return;
        }

        PlayerData executorData = SaveDataManager.getPlayerData(executor);

        if (executorData.getBlockedPlayers().contains(blockedPlayer.getUUID())) {
            // Executor has already blocked the other player
            executor.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.ALREADY_BLOCKED_PLAYER.get(),
                    Map.of(BLOCKED_PLAYER_CONSTANT, blockedPlayer.getName().getString()))));
            return;
        }

        executorData.getBlockedPlayers().add(blockedPlayer.getUUID()); // Add the blocked player to the executors list.

        executor.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.SENDER_BLOCKED_PLAYER.get(),
                Map.of(BLOCKED_PLAYER_CONSTANT, blockedPlayer.getName().getString()))));

        if (Config.SEND_BLOCKED_MESSAGES_TO_BOTH_PLAYERS.get()) {
            // Sending Blocked / Unblocked Messages has been enabled in the config
            blockedPlayer.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.PLAYER_BLOCKED_BY_SENDER.get(),
                    Map.of("sender_name", executor.getName().getString()))));
        }
    }

    public static void unBlockPlayer(ServerPlayer executor, ServerPlayer blockedPlayer) {
        if (RequestManager.isPlayerIdentical(executor, blockedPlayer)) {
            // Player is trying to block themselves
            executor.sendSystemMessage(Component.literal(Messages.CANNOT_UNBLOCK_SELF.get()));
            return;
        }

        PlayerData executorData = SaveDataManager.getPlayerData(executor);

        if (Boolean.FALSE.equals(executorData.getBlockedPlayers().contains(blockedPlayer.getUUID()))) {
            // Executor has not blocked the other player
            executor.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.HAVENT_BLOCKED_PLAYER.get(),
                    Map.of(BLOCKED_PLAYER_CONSTANT, blockedPlayer.getName().getString()))));
            return;
        }

        executorData.getBlockedPlayers().remove(blockedPlayer.getUUID()); // Remove the blocked player from the executors list.

        executor.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.SENDER_UNBLOCKED_PLAYER.get(),
                Map.of("unblocked_player", blockedPlayer.getName().getString()))));

        if (Config.SEND_BLOCKED_MESSAGES_TO_BOTH_PLAYERS.get()) {
            // Sending Blocked / Unblocked Messages has been enabled in the config
            blockedPlayer.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.PLAYER_UNBLOCKED_BY_SENDER.get(),
                    Map.of("sender_name", executor.getName().getString()))));
        }
    }

    private PlayerBlockingManager() {
    }
}
