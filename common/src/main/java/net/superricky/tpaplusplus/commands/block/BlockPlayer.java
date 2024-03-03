package net.superricky.tpaplusplus.commands.block;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.config.formatters.MessageParser;
import net.superricky.tpaplusplus.io.PlayerData;
import net.superricky.tpaplusplus.io.SaveDataManager;
import net.superricky.tpaplusplus.requests.RequestHelper;
import net.superricky.tpaplusplus.windupcooldown.cooldown.AsyncCooldownHelper;
import net.superricky.tpaplusplus.windupcooldown.cooldown.CooldownData;
import net.superricky.tpaplusplus.windupcooldown.windup.AsyncWindup;
import net.superricky.tpaplusplus.windupcooldown.CommandType;
import net.superricky.tpaplusplus.windupcooldown.windup.WindupData;

import java.util.Map;

public class BlockPlayer {
    public static void blockPlayer(ServerPlayer executor, ServerPlayer blockedPlayer) {
        // run the notify cooldown function and return if its false, to stop the player from blocking the player.
        if (!AsyncCooldownHelper.notifyAndCheckCooldown(executor, executor.getUUID(), CommandType.BLOCK)) return;

        if (RequestHelper.isPlayerIdentical(executor, blockedPlayer)) {
            // Player is trying to block themselves
            executor.sendSystemMessage(Component.literal(Messages.CANNOT_BLOCK_SELF.get()));
            return;
        }

        PlayerData executorData = SaveDataManager.getPlayerData(executor);

        if (executorData.getBlockedPlayers().contains(blockedPlayer.getUUID())) {
            // Executor has already blocked the other player
            executor.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.ALREADY_BLOCKED_PLAYER.get(),
                    Map.of(PlayerBlockingHelper.BLOCKED_PLAYER_CONSTANT, blockedPlayer.getName().getString()))));
            return;
        }

        if (Boolean.FALSE.equals(Config.ONLY_START_COOLDOWN_ON_COMMAND_SUCCESS.get()))
            AsyncCooldownHelper.getCooldownSet().add(new CooldownData(executor.getUUID(), CommandType.BLOCK, Config.BLOCK_COOLDOWN.get()));

        if (Config.BLOCK_WINDUP.get() == 0) {
            absoluteBlockPlayer(executorData, executor, blockedPlayer);
        } else {
            AsyncWindup.schedule(new WindupData(executorData, Config.BLOCK_WINDUP.get(), executor.getX(), executor.getY(), executor.getZ(), CommandType.BLOCK, new ServerPlayer[]{executor, blockedPlayer}));
        }
    }

    public static void absoluteBlockPlayer(PlayerData executorData, ServerPlayer executor, ServerPlayer blockedPlayer) {
        executorData.getBlockedPlayers().add(blockedPlayer.getUUID()); // Add the blocked player to the executors list.

        executor.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.SENDER_BLOCKED_PLAYER.get(),
                Map.of(PlayerBlockingHelper.BLOCKED_PLAYER_CONSTANT, blockedPlayer.getName().getString()))));

        if (Boolean.TRUE.equals(Config.SEND_BLOCKED_MESSAGES_TO_BOTH_PLAYERS.get())) {
            // Sending Blocked / Unblocked Messages has been enabled in the config
            blockedPlayer.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.PLAYER_BLOCKED_BY_SENDER.get(),
                    Map.of("sender_name", executor.getName().getString()))));
        }

        if (Boolean.TRUE.equals(Config.ONLY_START_COOLDOWN_ON_COMMAND_SUCCESS.get()))
            AsyncCooldownHelper.getCooldownSet().add(new CooldownData(executor.getUUID(), CommandType.BLOCK, Config.BLOCK_COOLDOWN.get()));
    }

    private BlockPlayer() {
    }
}
