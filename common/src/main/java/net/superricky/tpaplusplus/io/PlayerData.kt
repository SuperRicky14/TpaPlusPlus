package net.superricky.tpaplusplus.io;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

public class PlayerData {
    private volatile boolean tpToggle = false;
    private final List<UUID> blockedPlayers = Collections.synchronizedList(new ArrayList<>());

    public boolean getTPToggle() {
        return tpToggle;
    }

    public void setTPToggle(boolean tpToggle) {
        this.tpToggle = tpToggle;
    }

    public void addBlockedPlayer(UUID player) {
        blockedPlayers.add(player);
    }

    public void removeBlockedPlayer(UUID player) {
        blockedPlayers.remove(player);
    }

    public List<UUID> getBlockedPlayers() {
        return List.copyOf(blockedPlayers);
    }
}
