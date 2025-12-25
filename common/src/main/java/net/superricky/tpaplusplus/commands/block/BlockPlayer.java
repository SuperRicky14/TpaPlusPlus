package net.superricky.tpaplusplus.commands.block;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.io.PlayerData;
import net.superricky.tpaplusplus.io.SaveDataManager;
import net.superricky.tpaplusplus.requests.RequestHelper;
import net.superricky.tpaplusplus.util.MsgFmt;
import net.superricky.tpaplusplus.windupcooldown.cooldown.AsyncCooldownHelper;
import net.superricky.tpaplusplus.windupcooldown.cooldown.AsyncCooldownKt;
import net.superricky.tpaplusplus.windupcooldown.cooldown.CommandType;
import net.superricky.tpaplusplus.windupcooldown.windup.AsyncWindup;
import net.superricky.tpaplusplus.windupcooldown.windup.impl.BlockPlayerWindup;

import java.util.Map;

public class BlockPlayer {
    public static void blockPlayer(ServerPlayer executor, ServerPlayer blockedPlayer) {
        if (RequestHelper.isPlayerIdentical(executor, blockedPlayer)) {
            // Player is trying to block themselves
            executor.sendSystemMessage(Component.literal(Messages.CANNOT_BLOCK_SELF.get()));
            return;
        }

        PlayerData executorData = SaveDataManager.getPlayerData(executor);

        if (executorData.getBlockedPlayers().contains(blockedPlayer.getUUID())) {
            // Executor has already blocked the other player
            executor.sendSystemMessage(Component.literal(MsgFmt.fmt(Messages.ALREADY_BLOCKED_PLAYER.get(),
                    Map.of(PlayerBlockingHelper.BLOCKED_PLAYER_CONSTANT, blockedPlayer.getName().getString()))));
            return;
        }

        if (AsyncCooldownHelper.checkCommandCooldownAndNotify(executor, executor.getUUID(), CommandType.BLOCK))
            return;

        if (Config.BLOCK_COOLDOWN.get() > 0) // Check if cooldown is enabled
            AsyncCooldownKt.scheduleCooldown(executor.getUUID(), Config.BLOCK_COOLDOWN.get(), CommandType.BLOCK);

        if (Config.BLOCK_WINDUP.get() == 0) {
            absoluteBlockPlayer(executorData, executor, blockedPlayer);
        } else {
            AsyncWindup.INSTANCE.schedule(new BlockPlayerWindup(executorData, executor, blockedPlayer));
        }
    }

    public static void absoluteBlockPlayer(PlayerData executorData, ServerPlayer executor, ServerPlayer blockedPlayer) {
        executorData.addBlockedPlayer(blockedPlayer.getUUID()); // Add the blocked player to the executors list.

        executor.sendSystemMessage(Component.literal(MsgFmt.fmt(Messages.SENDER_BLOCKED_PLAYER.get(),
                Map.of(PlayerBlockingHelper.BLOCKED_PLAYER_CONSTANT, blockedPlayer.getName().getString()))));

        if (Boolean.TRUE.equals(Config.SEND_BLOCKED_MESSAGES_TO_BOTH_PLAYERS.get())) {
            // Sending Blocked / Unblocked Messages has been enabled in the config
            blockedPlayer.sendSystemMessage(Component.literal(MsgFmt.fmt(Messages.PLAYER_BLOCKED_BY_SENDER.get(),
                    Map.of("sender_name", executor.getName().getString()))));
        }
    }

    private BlockPlayer() {
    }
}
