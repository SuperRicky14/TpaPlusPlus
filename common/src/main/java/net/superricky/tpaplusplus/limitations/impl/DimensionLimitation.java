package net.superricky.tpaplusplus.limitations.impl;

import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.limitations.Limitation;

public class DimensionLimitation implements Limitation {
    @Override
    public boolean isViolated(ServerPlayer sender, ServerPlayer receiver) {
        return !sender.serverLevel().dimension().location().getPath()
                .equals(receiver.serverLevel().dimension().location().getPath())
                && !Config.ALLOW_INTER_DIMENSIONAL_TELEPORT.get();
    }

    @Override
    public String getViolationMessage(ServerPlayer sender, ServerPlayer receiver) {
        return String.format("Cannot teleport between dimensions: %s and %s",
                sender.serverLevel().dimension().location().getPath(),
                receiver.serverLevel().dimension().location().getPath());
    }
}
