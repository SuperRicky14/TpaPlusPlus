package net.superricky.tpaplusplus.commands.toggle;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.TPAPlusPlus;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.cooldown.CommandType;
import net.superricky.tpaplusplus.cooldown.CooldownData;
import net.superricky.tpaplusplus.cooldown.CooldownManager;
import net.superricky.tpaplusplus.io.PlayerData;
import net.superricky.tpaplusplus.io.SaveDataManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Duration;

public class TPToggle {
    private static final Logger LOGGER = LoggerFactory.getLogger(TPAPlusPlus.MOD_ID);

    public static void toggleTPOrWait(ServerPlayer executor) {
        CooldownData cooldown;
        if ((cooldown = CooldownManager.INSTANCE.getPlayerCooldown(executor.getUUID(), CommandType.TOGGLE)) != null) {
            CooldownManager.INSTANCE.notifyCooldown(executor, cooldown);
            return;
        }

        if (Config.TOGGLE_COOLDOWN.get() > 0) // Check if cooldown is enabled
            CooldownManager.INSTANCE.scheduleCooldown(executor.getUUID(), Duration.ofSeconds(Config.TOGGLE_COOLDOWN.get()), CommandType.TOGGLE);

        toggleTP(executor);
    }

    public static void toggleTP(ServerPlayer executor) {
        PlayerData executorData = SaveDataManager.getPlayerData(executor);

        executorData.setTPToggle(!executorData.getTPToggle()); // Switch the TPToggle of the player

        if (executorData.getTPToggle()) {
            executor.sendSystemMessage(Component.literal(Messages.TPTOGGLE_ENABLED.get()));
        } else {
            executor.sendSystemMessage(Component.literal(Messages.TPTOGGLE_DISABLED.get()));
        }
    }

    private TPToggle() {
    }
}
