package net.superricky.tpaplusplus.commands.toggle;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.io.PlayerData;
import net.superricky.tpaplusplus.io.SaveDataManager;
import net.superricky.tpaplusplus.windupcooldown.cooldown.AsyncCooldownHelper;
import net.superricky.tpaplusplus.windupcooldown.cooldown.CooldownData;
import net.superricky.tpaplusplus.windupcooldown.windup.AsyncWindup;
import net.superricky.tpaplusplus.windupcooldown.CommandType;
import net.superricky.tpaplusplus.windupcooldown.windup.WindupData;

public class TPToggle {
    public static void toggleTPOrWait(ServerPlayer executor) {
        // run the notify cooldown function and return if its false, to stop the player from toggling tp.
        if (!AsyncCooldownHelper.notifyAndCheckCooldown(executor, executor.getUUID(), CommandType.TOGGLE)) return;

        if (Boolean.FALSE.equals(Config.ONLY_START_COOLDOWN_ON_COMMAND_SUCCESS.get()))
            AsyncCooldownHelper.getCooldownSet().add(new CooldownData(executor.getUUID(), CommandType.TOGGLE, Config.TOGGLE_COOLDOWN.get()));

        if (Config.TOGGLE_WINDUP.get() == 0) {
            toggleTP(executor);
        } else {
            AsyncWindup.schedule(new WindupData(Config.TOGGLE_WINDUP.get(), executor.getX(), executor.getY(), executor.getZ(), CommandType.TOGGLE, new ServerPlayer[]{executor}));
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

        if (Boolean.TRUE.equals(Config.ONLY_START_COOLDOWN_ON_COMMAND_SUCCESS.get()))
            AsyncCooldownHelper.getCooldownSet().add(new CooldownData(executor.getUUID(), CommandType.TOGGLE, Config.TOGGLE_COOLDOWN.get()));
    }

    private TPToggle() {
    }
}
