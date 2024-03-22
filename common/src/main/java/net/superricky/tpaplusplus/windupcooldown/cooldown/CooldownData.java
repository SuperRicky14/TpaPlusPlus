package net.superricky.tpaplusplus.windupcooldown.cooldown;

import net.superricky.tpaplusplus.windupcooldown.CommandType;

import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

public class CooldownData {
    private final UUID playerOnCooldown;
    private final CommandType commandType;
    private final AtomicInteger delay;

    public CooldownData(UUID playerOnCooldown, CommandType commandType, int delay) {
        this.playerOnCooldown = playerOnCooldown;
        this.commandType = commandType;
        this.delay = new AtomicInteger(delay);
    }

    public UUID getPlayerOnCooldown() {
        return playerOnCooldown;
    }

    public CommandType getCommandType() {
        return commandType;
    }

    public int getDelay() {
        return delay.get();
    }

    public void setDelay(int delay) {
        this.delay.set(delay);
    }
}
