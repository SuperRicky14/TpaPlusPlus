package net.superricky.tpaplusplus.windupcooldown.cooldown;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.TPAPlusPlus;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.config.formatters.MessageParser;
import net.superricky.tpaplusplus.windupcooldown.CommandType;
import org.jetbrains.annotations.Nullable;

import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

public class AsyncCooldownHelper {
    private static final Set<CooldownData> cooldownSet = ConcurrentHashMap.newKeySet();

    static Set<CooldownData> getCooldownSet() {
        return cooldownSet;
    }

    /**
     * @return True if the command IS on cooldown, False if it ISN'T on cooldown.
     */
    public static boolean checkCommandCooldownAndNotify(ServerPlayer currentPlayer, UUID playerUUID, CommandType type) {
        @Nullable final CooldownData commandCooldown = getCommandCooldown(playerUUID, type);

        int commandCooldownDelay;

        if (Objects.isNull(commandCooldown)) {
            return false;
        } else {
            commandCooldownDelay = commandCooldown.getCooldownDelay();
        }

        if (commandCooldownDelay > 0) {
            currentPlayer.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.COMMAND_ON_COOLDOWN_MESSAGE.get(), Map.of("command_used", TPAPlusPlus.getCommandNameFromType(type), "time_remaining", String.valueOf(commandCooldownDelay)))));
            return true;
        }

        return false;
    }

    @Nullable
    public static CooldownData getCommandCooldown(UUID playerUUID, CommandType type) {
        for (CooldownData cooldown : cooldownSet) {
            if (cooldown.getPlayerCooldownUUID().equals(playerUUID) && cooldown.getCommandOnCooldown().equals(type)) {
                return cooldown;
            }
        }
        return null; // Return null if command is not on cooldown.
    }
}
