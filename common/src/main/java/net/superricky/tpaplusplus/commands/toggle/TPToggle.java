package net.superricky.tpaplusplus.commands.toggle;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.TPAPlusPlus;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.io.PlayerData;
import net.superricky.tpaplusplus.io.SaveDataManager;
import net.superricky.tpaplusplus.windupcooldown.CommandType;
import net.superricky.tpaplusplus.windupcooldown.cooldown.AsyncCooldownHelper;
import net.superricky.tpaplusplus.windupcooldown.cooldown.AsyncCooldownKt;
import net.superricky.tpaplusplus.windupcooldown.windup.AsyncWindupKt;
import net.superricky.tpaplusplus.windupcooldown.windup.impl.TPToggleWindup;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TPToggle {
    private static final Logger LOGGER = LoggerFactory.getLogger(TPAPlusPlus.MOD_ID);

    public static void toggleTPOrWait(ServerPlayer executor) {
        try {
            if (AsyncCooldownHelper.checkCommandCooldownAndNotify(executor, executor.getUUID(), CommandType.TOGGLE))
                return;
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
            executor.sendSystemMessage(Component.literal(e.getMessage()));
            return;
        }


        if (Config.TOGGLE_COOLDOWN.get() > 0) // Check if cooldown is enabled
            AsyncCooldownKt.scheduleCooldown(executor.getUUID(), Config.TOGGLE_COOLDOWN.get(), CommandType.TOGGLE);

        if (Config.TOGGLE_WINDUP.get() == 0) {
            toggleTP(executor);
        } else {
            AsyncWindupKt.schedule(new TPToggleWindup(executor));
        }
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
