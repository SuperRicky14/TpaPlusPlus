package net.superricky.tpaplusplus.limitations;

import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.limitations.impl.DimensionLimitation;
import net.superricky.tpaplusplus.limitations.impl.DistanceLimitation;

import java.util.ArrayList;
import java.util.List;

public class LimitationManager {
    private final List<Limitation> limitations;

    public LimitationManager() {
        limitations = new ArrayList<>();
        limitations.add(new DimensionLimitation());
        limitations.add(new DistanceLimitation());
    }

    public boolean canTeleport(ServerPlayer sender, ServerPlayer receiver) {
        return limitations.stream().noneMatch(l -> l.isViolated(sender, receiver));
    }

    public List<String> getViolationMessages(ServerPlayer sender, ServerPlayer receiver) {
        return limitations.stream()
                .filter(l -> l.isViolated(sender, receiver))
                .map(l -> l.getViolationMessage(sender, receiver))
                .toList();
    }
}