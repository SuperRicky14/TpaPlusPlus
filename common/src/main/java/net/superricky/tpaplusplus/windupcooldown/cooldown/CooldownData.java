package net.superricky.tpaplusplus.windupcooldown.cooldown;

import net.superricky.tpaplusplus.windupcooldown.CommandType;

import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

public class CooldownData {
    private final UUID playerCooldownUUID;
    private final AtomicInteger cooldownDelay;

    public CommandType getCommandOnCooldown() {
        return commandOnCooldown;
    }

    private final CommandType commandOnCooldown;

    public UUID getPlayerCooldownUUID() {
        return playerCooldownUUID;
    }

    public int getCooldownDelay() {
        return cooldownDelay.get();
    }

    public void setCooldownDelay(int cooldownDelay) {
        this.cooldownDelay.set(cooldownDelay);
    }

    public CooldownData(UUID playerUUID, int cooldownDelay, CommandType commandOnCooldown) {
        this.playerCooldownUUID = playerUUID;
        this.cooldownDelay = new AtomicInteger(cooldownDelay);
        this.commandOnCooldown = commandOnCooldown;
    }
}
