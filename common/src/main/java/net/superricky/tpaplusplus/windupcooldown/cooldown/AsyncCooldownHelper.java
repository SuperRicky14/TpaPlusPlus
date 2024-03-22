package net.superricky.tpaplusplus.windupcooldown.cooldown;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.TPAPlusPlus;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.config.formatters.MessageParser;
import net.superricky.tpaplusplus.windupcooldown.CommandType;
import org.jetbrains.annotations.Nullable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

public class AsyncCooldownHelper {
    private static final Logger LOGGER = LoggerFactory.getLogger(TPAPlusPlus.MOD_ID);
    private static final Set<CooldownData> cooldownSet = ConcurrentHashMap.newKeySet();

    public static Set<CooldownData> getCooldownSet() {
        return cooldownSet;
    }

    public static void postCooldown(UUID playerOnCooldown, CommandType type, int delay) {
        if (delay != 0) {
            cooldownSet.add(new CooldownData(playerOnCooldown, type, delay));
        }

        if (Config.GLOBAL_COOLDOWN.get() != 0) {
            cooldownSet.add(new CooldownData(playerOnCooldown, CommandType.GLOBAL, Config.GLOBAL_COOLDOWN.get()));
        }
    }

    /**
     * @return TRUE if the player passed the cooldown check, FALSE if they failed
     */
    public static boolean notifyAndCheckCooldown(ServerPlayer playerToNotify, UUID playerToGrab, CommandType commandToCheck, boolean isHereRequest) {
        if (!(commandToCheck.equals(CommandType.SEND))) {
            LOGGER.error("IllegalArgumentException: Wrong Commmand Type when received \"isHereRequest\" parameter! Please report this issue to the TPA++ issue page immediately.");
            throw new IllegalArgumentException("Wrong Commmand Type when received \"isHereRequest\" parameter! Please report this issue to the TPA++ issue page immediately.");
        }

        CooldownData cooldownData = getCooldownData(playerToGrab, commandToCheck);

        if (cooldownData == null) {
            return true;
        }

        playerToNotify.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.ERR_COMMAND_ON_COOLDOWN_MESSAGE.get(), Map.of("command_used", TPAPlusPlus.getCommandNameFromType(commandToCheck, isHereRequest)), Map.of("time_remaining", cooldownData.getDelay()))));
        return false;
    }

    /**
     * @return TRUE if the player passed the cooldown check, FALSE if they failed
     */
    public static boolean notifyAndCheckCooldown(ServerPlayer playerToNotify, UUID playerToGrab, CommandType commandToCheck) {
        CooldownData cooldownData = getCooldownData(playerToGrab, commandToCheck);
        CooldownData globalCooldownData = getCooldownData(playerToGrab, CommandType.GLOBAL);

        if (cooldownData == null && globalCooldownData == null) {
            return true;
        }

        if (cooldownData.getCommandType().equals(CommandType.SEND)) {
            LOGGER.error("IllegalArgumentException: Missing parameter \"isHereRequest\", when the CommandType was SEND! Please report this issue to the TPA++ issue page immediately.");
            throw new IllegalArgumentException("Missing parameter \"isHereRequest\", when the CommandType was SEND! Please report this issue to the TPA++ issue page immediately.");
        }

        if (cooldownData != null)
            playerToNotify.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.ERR_COMMAND_ON_COOLDOWN_MESSAGE.get(), Map.of("command_used", TPAPlusPlus.getCommandNameFromType(commandToCheck)), Map.of("time_remaining", cooldownData.getDelay()))));
        if (globalCooldownData != null)
            playerToNotify.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.ERR_COMMAND_ON_COOLDOWN_MESSAGE.get(), Map.of("command_used", TPAPlusPlus.getCommandNameFromType(commandToCheck)), Map.of("time_remaining", cooldownData.getDelay()))));
        return false;
    }

    @Nullable
    public static CooldownData getCooldownData(UUID playerToGrab, CommandType commandToCheck) {
        for (CooldownData cooldownData : cooldownSet) {
            if (cooldownData.getPlayerOnCooldown().equals(playerToGrab)
                && cooldownData.getCommandType().equals(commandToCheck)) {
                return cooldownData;
            }
        }
        return null;
    }

    private AsyncCooldownHelper() {
    }
}
