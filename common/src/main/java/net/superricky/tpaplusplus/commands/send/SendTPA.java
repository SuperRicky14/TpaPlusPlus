package net.superricky.tpaplusplus.commands.send;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.commands.block.PlayerBlockingHelper;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.io.PlayerData;
import net.superricky.tpaplusplus.io.SaveDataManager;
import net.superricky.tpaplusplus.limitations.LimitationManager;
import net.superricky.tpaplusplus.requests.Request;
import net.superricky.tpaplusplus.requests.RequestHelper;
import net.superricky.tpaplusplus.timeout.TimeoutManagerKt;
import net.superricky.tpaplusplus.util.MsgFmt;
import net.superricky.tpaplusplus.windupcooldown.cooldown.AsyncCooldownHelper;
import net.superricky.tpaplusplus.windupcooldown.cooldown.AsyncCooldownKt;
import net.superricky.tpaplusplus.windupcooldown.cooldown.CommandType;
import net.superricky.tpaplusplus.windupcooldown.windup.AsyncWindup;
import net.superricky.tpaplusplus.windupcooldown.windup.impl.TPAHereWindup;
import net.superricky.tpaplusplus.windupcooldown.windup.impl.TPAWindup;

import java.util.Map;
import java.util.Objects;

public class SendTPA {
    public static void sendTeleportRequest(ServerPlayer sender, ServerPlayer receiver, boolean isHereRequest) {
        if (RequestHelper.isPlayerIdentical(sender, receiver)) {
            sender.sendSystemMessage(Component.literal(Messages.ERR_NO_SELF_TELEPORT.get()));
            return;
        }

        if (RequestHelper.alreadySentTeleportRequest(sender, receiver)) {
            sender.sendSystemMessage(Component.literal(MsgFmt.fmt(Messages.ERR_ALREADY_SENT_TELEPORT_REQUEST.get(), Map.of("receiver_name", receiver.getName().getString()))));
            return;
        }

        if (PlayerBlockingHelper.isPlayerBlocked(sender, receiver))
            return; // Return if one of the players has blocked the other player.

        PlayerData receiverData = SaveDataManager.getPlayerData(receiver);

        if (Boolean.FALSE.equals(Objects.isNull(receiverData)) && receiverData.getTPToggle()) { // receiverData is not null && receiver TP toggle is enabled.
            sender.sendSystemMessage(Component.literal(MsgFmt.fmt(Messages.ERR_RECEIVER_TP_DISABLED.get(),
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

        if (!LimitationManager.canTeleport(sender, receiver)) {
            String[] violationMessages = LimitationManager.getViolationMessages(sender, receiver);
            for (String message : violationMessages) {
                sender.sendSystemMessage(Component.literal(message));
            }
            return;
        }

        if (isHereRequest) {
            if (AsyncCooldownHelper.checkCommandCooldownAndNotify(sender, sender.getUUID(), CommandType.TPAHERE))
                return;

            if (Config.TPAHERE_COOLDOWN.get() > 0) // Check if cooldown is enabled
                AsyncCooldownKt.scheduleCooldown(sender.getUUID(), Config.TPAHERE_COOLDOWN.get(), CommandType.TPAHERE);

            if (Config.TPAHERE_WINDUP.get() == 0) {
                absoluteSendTeleportRequest(sender, receiver, isHereRequest);
            } else {
                AsyncWindup.INSTANCE.schedule(new TPAHereWindup(sender, receiver));
            }
        } else {
            if (AsyncCooldownHelper.checkCommandCooldownAndNotify(sender, sender.getUUID(), CommandType.TPA))
                return;

            if (Config.TPA_COOLDOWN.get() > 0) // Check if cooldown is enabled
                AsyncCooldownKt.scheduleCooldown(sender.getUUID(), Config.TPA_COOLDOWN.get(), CommandType.TPA);

            if (Config.TPA_WINDUP.get() == 0) {
                absoluteSendTeleportRequest(sender, receiver, isHereRequest);
            } else {
                AsyncWindup.INSTANCE.schedule(new TPAWindup(sender, receiver));
            }
        }
    }

    public static void absoluteSendTeleportRequest(ServerPlayer sender, ServerPlayer receiver, boolean isHereRequest) {
        Request request = new Request(sender, receiver, isHereRequest);

        RequestHelper.getRequestSet().add(request);

        if (!Objects.equals(Config.TPA_TIMEOUT_IN_SECONDS.get(), Config.TPA_TIMEOUT_DISABLED)) {
            TimeoutManagerKt.scheduleTeleportTimeout(request, Config.TPA_TIMEOUT_IN_SECONDS.get());
        }


        if (isHereRequest) {
            sender.sendSystemMessage(Component.literal(MsgFmt.fmt(Messages.SENDER_SENT_TPAHERE.get(), Map.of("receivers_name", receiver.getName().getString()))));
            receiver.sendSystemMessage(Component.literal(MsgFmt.fmt(Messages.RECEIVER_GOT_TPAHERE.get(), Map.of("senders_name", sender.getName().getString()))));
        } else {
            sender.sendSystemMessage(Component.literal(MsgFmt.fmt(Messages.SENDER_SENT_TPA.get(), Map.of("receivers_name", receiver.getName().getString()))));
            receiver.sendSystemMessage(Component.literal(MsgFmt.fmt(Messages.RECEIVER_GOT_TPA.get(), Map.of("senders_name", sender.getName().getString()))));
        }
    }

    private SendTPA() {
    }
}
