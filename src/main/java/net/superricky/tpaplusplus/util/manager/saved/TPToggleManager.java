package net.superricky.tpaplusplus.util.manager.saved;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.util.configuration.Messages;

import java.util.*;

public class TPToggleManager {
    public static void toggleTP(ServerPlayer executor) {
        UUID executorUUID = executor.getUUID();
        PlayerData executorData = SaveDataManager.playerDataMap.get(executorUUID);

        // Create executorData if it doesn't already exist in the map.
        if (Objects.isNull(executorData)) executorData = new PlayerData();

        executorData.setTPToggle(!executorData.getTPToggle()); // Switch the TPToggle of the player

        SaveDataManager.playerDataMap.remove(executorUUID);

        SaveDataManager.playerDataMap.put(executorUUID, executorData);

        if (executorData.getTPToggle()) {
            executor.sendSystemMessage(Component.literal(Messages.TPTOGGLE_ENABLED.get()));
        } else {
            executor.sendSystemMessage(Component.literal(Messages.TPTOGGLE_DISABLED.get()));
        }
    }

    private TPToggleManager() {
    }
}
