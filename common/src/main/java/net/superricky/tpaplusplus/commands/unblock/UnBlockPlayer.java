package net.superricky.tpaplusplus.commands.unblock;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.cooldown.CommandType;
import net.superricky.tpaplusplus.cooldown.CooldownData;
import net.superricky.tpaplusplus.cooldown.CooldownManager;
import net.superricky.tpaplusplus.io.PlayerData;
import net.superricky.tpaplusplus.io.SaveDataManager;
import net.superricky.tpaplusplus.requests.RequestHelper;
import net.superricky.tpaplusplus.util.MsgFmtKt;

import java.time.Duration;
import java.util.Map;

public class UnBlockPlayer {
    public static void unBlockPlayer(ServerPlayer executor, ServerPlayer blockedPlayer) {
        if (RequestHelper.INSTANCE.isPlayerIdentical(executor, blockedPlayer)) {
            // Player is trying to block themselves
            executor.sendSystemMessage(Component.literal(Messages.CANNOT_UNBLOCK_SELF.get()));
            return;
        }

        PlayerData executorData = SaveDataManager.INSTANCE.getPlayerData(executor.getUUID());

        if (!executorData.hasBlockedPlayer(blockedPlayer.getUUID())) {
            // Executor has not blocked the other player
            executor.sendSystemMessage(Component.literal(MsgFmtKt.template(Messages.HAVENT_BLOCKED_PLAYER.get(),
                    Map.of("blocked_player", blockedPlayer.getName().getString()))));
            return;
        }

        CooldownData cooldown;
        if ((cooldown = CooldownManager.INSTANCE.getPlayerCooldown(executor.getUUID(), CommandType.UNBLOCK)) != null) {
            CooldownManager.INSTANCE.notifyCooldown(executor, cooldown);
            return;
        }

        if (Config.UNBLOCK_COOLDOWN.get() > 0) // Check if cooldown is enabled
            CooldownManager.INSTANCE.scheduleCooldown(executor.getUUID(), Duration.ofSeconds(Config.UNBLOCK_COOLDOWN.get()), CommandType.UNBLOCK);

        absoluteUnBlockPlayer(executor, blockedPlayer);
    }

    public static void absoluteUnBlockPlayer(ServerPlayer executor, ServerPlayer blockedPlayer) {
        SaveDataManager.INSTANCE.removeBlockedPlayer(executor.getUUID(), blockedPlayer.getUUID());

        executor.sendSystemMessage(Component.literal(MsgFmtKt.template(Messages.SENDER_UNBLOCKED_PLAYER.get(),
                Map.of("unblocked_player", blockedPlayer.getName().getString()))));

        if (Boolean.TRUE.equals(Config.SEND_BLOCKED_MESSAGES_TO_BOTH_PLAYERS.get())) {
            // Sending Blocked / Unblocked Messages has been enabled in the config
            blockedPlayer.sendSystemMessage(Component.literal(MsgFmtKt.template(Messages.PLAYER_UNBLOCKED_BY_SENDER.get(),
                    Map.of("sender_name", executor.getName().getString()))));
        }
    }

    private UnBlockPlayer() {
    }
}
