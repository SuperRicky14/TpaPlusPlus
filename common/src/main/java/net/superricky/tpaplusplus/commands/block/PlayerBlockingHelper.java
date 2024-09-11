package net.superricky.tpaplusplus.commands.block;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.util.MsgFmt;
import net.superricky.tpaplusplus.io.SaveDataManager;

import java.util.Map;
import java.util.UUID;

public class PlayerBlockingHelper {
    public static final String BLOCKED_PLAYER_CONSTANT = "blocked_player";

    public static boolean isPlayerBlocked(ServerPlayer sender, ServerPlayer receiver) {
        for (UUID senderBlockedUUID : SaveDataManager.getPlayerData(sender).getBlockedPlayers()) {
            if (receiver.getUUID().equals(senderBlockedUUID)) {
                // The sender blocked the receiver
                sender.sendSystemMessage(Component.literal(MsgFmt.fmt(Messages.SENDER_BLOCKED_RECEIVER.get(),
                        Map.of(BLOCKED_PLAYER_CONSTANT, receiver.getName().getString()))));
                return true;
            }
        }

        for (UUID receiverBlockedUUID : SaveDataManager.getPlayerData(receiver).getBlockedPlayers()) {
            if (sender.getUUID().equals(receiverBlockedUUID)) {
                // The receiver blocked the sender
                sender.sendSystemMessage(Component.literal(MsgFmt.fmt(Messages.RECEIVER_BLOCKED_SENDER.get(),
                        Map.of("blocking_player", receiver.getName().getString()))));
                return true;
            }
        }
        return false;
    }

    private PlayerBlockingHelper() {
    }
}
