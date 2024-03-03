package net.superricky.tpaplusplus.windupcooldown.cooldown;

import net.superricky.tpaplusplus.windupcooldown.CommandType;

import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

public class CooldownData {
    private final UUID affectedPlayer;
    private final CommandType commandType;
    private final AtomicInteger delay;

    public CooldownData(UUID affectedPlayer, CommandType commandType, int delay) {
        this.affectedPlayer = affectedPlayer;
        this.commandType = commandType;
        this.delay = new AtomicInteger(delay);
    }

    public UUID getAffectedPlayer() {
        return affectedPlayer;
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
