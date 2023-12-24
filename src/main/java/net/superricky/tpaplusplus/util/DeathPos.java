package net.superricky.tpaplusplus.util;

import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.phys.Vec3;
import org.joml.Vector3f;

public class DeathPos extends Vec3 {
    public DeathPos(ServerLevel deathLevel, double pX, double pY, double pZ) {
        super(pX, pY, pZ);
        this.deathLevel = deathLevel;
    }

    public DeathPos(ServerLevel deathLevel, Vector3f pVector) {
        super(pVector);
        this.deathLevel = deathLevel;
    }

    public ServerLevel getDeathLevel() {
        return deathLevel;
    }

    private final ServerLevel deathLevel;
}
