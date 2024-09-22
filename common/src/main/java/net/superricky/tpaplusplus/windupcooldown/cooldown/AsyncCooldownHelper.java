package net.superricky.tpaplusplus.windupcooldown.cooldown;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.util.MsgFmt;
import org.jetbrains.annotations.Nullable;

import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

public class AsyncCooldownHelper {
    private static final Set<CooldownData> cooldownSet = ConcurrentHashMap.newKeySet();

    private static String getCommandNameFromType(CommandType commandType) {
        switch (commandType) {
            case BACK -> {
                return Config.BACK_COMMAND_NAME.get();
            }
            case ACCEPT -> {
                return Config.TPAACCEPT_COMMAND_NAME.get();
            }
            case DENY -> {
                return Config.TPADENY_COMMAND_NAME.get();
            }
            case CANCEL -> {
                return Config.TPACANCEL_COMMAND_NAME.get();
            }
            case TPA -> {
                return Config.TPA_COMMAND_NAME.get();
            }
            case TPAHERE -> {
                return Config.TPAHERE_COMMAND_NAME.get();
            }
            case BLOCK -> {
                return Config.TPBLOCK_COMMAND_NAME.get();
            }
            case TOGGLE -> {
                return Config.TPTOGGLE_COMMAND_NAME.get();
            }
            case UNBLOCK -> {
                return Config.TPUNBLOCK_COMMAND_NAME.get();
            }
        }

        throw new AssertionError("Unreachable code reached: Could not find name for Command Type \"" + commandType + "\"!");
    }

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
            currentPlayer.sendSystemMessage(Component.literal(MsgFmt.fmt(Messages.COMMAND_ON_COOLDOWN_MESSAGE.get(), Map.of("command_used", getCommandNameFromType(type), "time_remaining", String.valueOf(commandCooldownDelay)))));
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
