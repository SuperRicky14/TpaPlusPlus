package net.superricky.tpaplusplus.util.manager.saved;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class PlayerData {
    private boolean tpToggle = false;
    private final List<UUID> blockedPlayers = new ArrayList<>();

    public boolean getTPToggle() {
        return tpToggle;
    }

    public void setTPToggle(boolean tpToggle) {
        this.tpToggle = tpToggle;
    }

    public List<UUID> getBlockedPlayers() {
        return blockedPlayers;
    }
}
