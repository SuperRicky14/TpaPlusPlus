package net.superricky.tpaplusplus.commands.send;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.commands.block.PlayerBlockingHelper;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.config.formatters.MessageParser;
import net.superricky.tpaplusplus.io.PlayerData;
import net.superricky.tpaplusplus.io.SaveDataManager;
import net.superricky.tpaplusplus.limitations.LimitationManager;
import net.superricky.tpaplusplus.requests.Request;
import net.superricky.tpaplusplus.requests.RequestHelper;
import net.superricky.tpaplusplus.timeout.TimeoutScheduler;
import net.superricky.tpaplusplus.windupcooldown.cooldown.AsyncCooldownHelper;
import net.superricky.tpaplusplus.windupcooldown.cooldown.CooldownData;
import net.superricky.tpaplusplus.windupcooldown.windup.AsyncWindup;
import net.superricky.tpaplusplus.windupcooldown.CommandType;
import net.superricky.tpaplusplus.windupcooldown.windup.WindupData;

import java.util.Map;
import java.util.Objects;

public class SendTPA {
    public static void sendTeleportRequest(ServerPlayer sender, ServerPlayer receiver, boolean isHereRequest) {
        // run the notify cooldown function and return if its false, to stop the player from sending the request.
        if (!AsyncCooldownHelper.notifyAndCheckCooldown(sender, sender.getUUID(), CommandType.SEND, isHereRequest)) return;

        if (RequestHelper.isPlayerIdentical(sender, receiver)) {
            sender.sendSystemMessage(Component.literal(Messages.ERR_NO_SELF_TELEPORT.get()));
            return;
        }

        if (RequestHelper.alreadySentTeleportRequest(sender, receiver)) {
            sender.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.ERR_ALREADY_SENT_TELEPORT_REQUEST.get(), Map.of("receiver_name", receiver.getName().getString()))));
            return;
        }

        if (PlayerBlockingHelper.isPlayerBlocked(sender, receiver)) return; // Return if one of the players has blocked the other player.

        PlayerData receiverData = SaveDataManager.getPlayerData(receiver);

        if (Boolean.FALSE.equals(Objects.isNull(receiverData)) && receiverData.getTPToggle()) { // receiverData is not null && receiver TP toggle is enabled.
            sender.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.ERR_RECEIVER_TP_DISABLED.get(),
                    Map.of("receiverName", receiver.getName().getString()))));
            return;
        }

        if (Boolean.FALSE.equals(Config.ALLOW_TPTOGGLED_PLAYERS_TO_SEND_REQUESTS.get())) { // Allow TPToggled players to send requests is disabled in the config
            PlayerData senderData = SaveDataManager.getPlayerData(sender);

            if (Boolean.FALSE.equals(Objects.isNull(senderData)) && senderData.getTPToggle()) { // senderData is not null && sender TP toggle is enabled.
                sender.sendSystemMessage(Component.literal(Messages.ERR_SENDER_TP_DISABLED.get()));
                return;
            }
        }

        // run the notify function and return if it false, to stop the player from sending the request.
        if (!LimitationManager.notifyAndCheckAllowedToTeleport(sender, receiver, false)) return;

        if (Boolean.FALSE.equals(Config.ONLY_START_COOLDOWN_ON_COMMAND_SUCCESS.get()))
            AsyncCooldownHelper.getCooldownSet().add(new CooldownData(sender.getUUID(), CommandType.SEND, Config.SEND_COOLDOWN.get()));

        if (Config.SEND_WINDUP.get() == 0) {
            absoluteSendTeleportRequest(sender, receiver, isHereRequest);
        } else {
            AsyncWindup.schedule(new WindupData(isHereRequest, Config.SEND_WINDUP.get(), sender.getX(), sender.getY(), sender.getZ(), CommandType.SEND, new ServerPlayer[]{sender, receiver}));
        }
    }

    public static void absoluteSendTeleportRequest(ServerPlayer sender, ServerPlayer receiver, boolean isHereRequest) {
        Request request = new Request(sender, receiver, isHereRequest);

        RequestHelper.getRequestSet().add(request);

        TimeoutScheduler.scheduleTeleportTimeout(request);

        if (isHereRequest) {
            sender.sendSystemMessage(Component.literal(String.format(Messages.SENDER_SENT_TPAHERE.get(), receiver.getName().getString())));
            receiver.sendSystemMessage(Component.literal(String.format(Messages.RECEIVER_GOT_TPAHERE.get(), sender.getName().getString())));
        } else {
            sender.sendSystemMessage(Component.literal(String.format(Messages.SENDER_SENT_TPA.get(), receiver.getName().getString())));
            receiver.sendSystemMessage(Component.literal(String.format(Messages.RECEIVER_GOT_TPA.get(), sender.getName().getString())));
        }

        if (Boolean.TRUE.equals(Config.ONLY_START_COOLDOWN_ON_COMMAND_SUCCESS.get()))
            AsyncCooldownHelper.getCooldownSet().add(new CooldownData(sender.getUUID(), CommandType.SEND, Config.SEND_COOLDOWN.get()));
    }

    private SendTPA() {
    }
}
