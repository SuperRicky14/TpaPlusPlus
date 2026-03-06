package net.superricky.tpaplusplus.commands.send;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.cooldown.CommandType;
import net.superricky.tpaplusplus.cooldown.CooldownData;
import net.superricky.tpaplusplus.cooldown.CooldownManager;
import net.superricky.tpaplusplus.io.PlayerData;
import net.superricky.tpaplusplus.io.SaveDataManager;
import net.superricky.tpaplusplus.limitations.LimitationManager;
import net.superricky.tpaplusplus.requests.Request;
import net.superricky.tpaplusplus.requests.RequestHelper;
import net.superricky.tpaplusplus.timeout.TimeoutManager;
import net.superricky.tpaplusplus.util.MsgFmtKt;

import java.time.Duration;
import java.util.Map;
import java.util.Objects;

public class SendTPA {
    private static boolean isEitherBlocked(ServerPlayer sender, ServerPlayer receiver) {
        PlayerData senderData = SaveDataManager.INSTANCE.getPlayerData(sender.getUUID());
        if (senderData.hasBlockedPlayer(receiver.getUUID())) {
            sender.sendSystemMessage(Component.literal(MsgFmtKt.template(Messages.SENDER_BLOCKED_RECEIVER.get(),
                    Map.of("blocked_player", receiver.getName().getString()))));
            return true;
        }

        PlayerData receiverData = SaveDataManager.INSTANCE.getPlayerData(receiver.getUUID());
        if (receiverData.hasBlockedPlayer(sender.getUUID())) {
            sender.sendSystemMessage(Component.literal(MsgFmtKt.template(Messages.RECEIVER_BLOCKED_SENDER.get(),
                    Map.of("blocking_player", receiver.getName().getString()))));
            return true;
        }

        return false;
    }

    public static void sendTeleportRequest(ServerPlayer sender, ServerPlayer receiver, boolean isHereRequest) {
        if (RequestHelper.INSTANCE.isPlayerIdentical(sender, receiver)) {
            sender.sendSystemMessage(Component.literal(Messages.ERR_NO_SELF_TELEPORT.get()));
            return;
        }

        if (RequestHelper.INSTANCE.alreadySentTeleportRequest(sender, receiver)) {
            sender.sendSystemMessage(Component.literal(MsgFmtKt.template(Messages.ERR_ALREADY_SENT_TELEPORT_REQUEST.get(), Map.of("receiver_name", receiver.getName().getString()))));
            return;
        }

        if (isEitherBlocked(sender, receiver)) return;

        PlayerData receiverData = SaveDataManager.INSTANCE.getPlayerData(receiver.getUUID());
        if (receiverData.getTpToggle()) { // receiverData is not null && receiver TP toggle is enabled.
            sender.sendSystemMessage(Component.literal(MsgFmtKt.template(Messages.ERR_RECEIVER_TP_DISABLED.get(),
                    Map.of("receiverName", receiver.getName().getString()))));
            return;
        }

        if (Boolean.FALSE.equals(Config.ALLOW_TPTOGGLED_PLAYERS_TO_SEND_REQUESTS.get())) { // Allow TPToggled players to send requests is disabled in the config
            PlayerData senderData = SaveDataManager.INSTANCE.getPlayerData(sender.getUUID());

            if (senderData.getTpToggle()) { // senderData is not null && sender TP toggle is enabled.
                sender.sendSystemMessage(Component.literal(Messages.ERR_SENDER_TP_DISABLED.get()));
                return;
            }
        }

        if (!LimitationManager.canTeleport(sender, receiver)) {
            String[] violationMessages = LimitationManager.getViolationMessages(sender, receiver);
            for (String message : violationMessages) {
                sender.sendSystemMessage(Component.literal(message));
            }
            return;
        }

        if (isHereRequest) {
            CooldownData cooldown;
            if ((cooldown = CooldownManager.INSTANCE.getPlayerCooldown(sender.getUUID(), CommandType.TPAHERE)) != null) {
                CooldownManager.INSTANCE.notifyCooldown(sender, cooldown);
                return;
            }

            if (Config.TPAHERE_COOLDOWN.get() > 0) // Check if cooldown is enabled
                CooldownManager.INSTANCE.scheduleCooldown(sender.getUUID(), Duration.ofSeconds(Config.TPAHERE_COOLDOWN.get()), CommandType.TPAHERE);

            absoluteSendTeleportRequest(sender, receiver, isHereRequest);
        } else {
            CooldownData cooldown;
            if ((cooldown = CooldownManager.INSTANCE.getPlayerCooldown(sender.getUUID(), CommandType.TPA)) != null) {
                CooldownManager.INSTANCE.notifyCooldown(sender, cooldown);
                return;
            }

            if (Config.TPA_COOLDOWN.get() > 0) // Check if cooldown is enabled
                CooldownManager.INSTANCE.scheduleCooldown(sender.getUUID(), Duration.ofSeconds(Config.TPA_COOLDOWN.get()), CommandType.TPA);

            absoluteSendTeleportRequest(sender, receiver, isHereRequest);
        }
    }

    public static void absoluteSendTeleportRequest(ServerPlayer sender, ServerPlayer receiver, boolean isHereRequest) {
        Request request = new Request(sender, receiver, isHereRequest);

        RequestHelper.INSTANCE.getRequestSet().add(request);

        if (!Objects.equals(Config.TPA_TIMEOUT_IN_SECONDS.get(), Config.TPA_TIMEOUT_DISABLED)) {
            TimeoutManager.INSTANCE.scheduleTeleportTimeout(request, Duration.ofSeconds(Config.TPA_TIMEOUT_IN_SECONDS.get()));
        }


        if (isHereRequest) {
            sender.sendSystemMessage(Component.literal(MsgFmtKt.template(Messages.SENDER_SENT_TPAHERE.get(), Map.of("receivers_name", receiver.getName().getString()))));
            receiver.sendSystemMessage(Component.literal(MsgFmtKt.template(Messages.RECEIVER_GOT_TPAHERE.get(), Map.of("senders_name", sender.getName().getString()))));
        } else {
            sender.sendSystemMessage(Component.literal(MsgFmtKt.template(Messages.SENDER_SENT_TPA.get(), Map.of("receivers_name", receiver.getName().getString()))));
            receiver.sendSystemMessage(Component.literal(MsgFmtKt.template(Messages.RECEIVER_GOT_TPA.get(), Map.of("senders_name", sender.getName().getString()))));
        }
    }

    private SendTPA() {
    }
}
