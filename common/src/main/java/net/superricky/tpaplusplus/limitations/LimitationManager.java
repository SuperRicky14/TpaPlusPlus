package net.superricky.tpaplusplus.limitations;

import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.limitations.impl.DimensionLimitation;
import net.superricky.tpaplusplus.limitations.impl.DistanceLimitation;

import java.util.List;

public class LimitationManager {
    private static final List<Limitation> limitations = List.of(new DimensionLimitation(), new DistanceLimitation());

    public static boolean canTeleport(ServerPlayer sender, ServerPlayer receiver) {
        return limitations.stream().noneMatch(l -> l.isViolated(sender, receiver));
    }

    public static String[] getViolationMessages(ServerPlayer sender, ServerPlayer receiver) {
        return limitations.stream()
                .filter(l -> l.isViolated(sender, receiver))
                .map(l -> l.getViolationMessage(sender, receiver))
                .toArray(String[]::new);
    }

    private LimitationManager() {
    }
}