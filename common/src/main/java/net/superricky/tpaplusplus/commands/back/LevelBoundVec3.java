package net.superricky.tpaplusplus.commands.back;

import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.phys.Vec3;

import java.util.Objects;

public class LevelBoundVec3 extends Vec3 {
    private final ServerLevel serverLevel;

    public LevelBoundVec3(ServerLevel serverLevel, double pX, double pY, double pZ) {
        super(pX, pY, pZ);
        this.serverLevel = serverLevel;
    }

    public ServerLevel serverLevel() {
        return serverLevel;
    }

    // Object methods.
    @Override
    public boolean equals(Object o) {
        if (this == o) return true; // self-equality
        if (!(o instanceof LevelBoundVec3 that)) return false; // ensure object is LevelBoundVec3

        // Compare every field for equality:
        return Double.compare(x, that.x) == 0 &&
                Double.compare(y, that.y) == 0 &&
                Double.compare(z, that.z) == 0 &&
                Objects.equals(serverLevel, that.serverLevel);
    }

    @Override
    public int hashCode() {
        int parentHash = super.hashCode(); // Get hash code from parent class
        int result = Objects.hash(serverLevel); // Incorporate serverLevel
        result = 31 * result + parentHash; // Combine with parent hash code
        return result;
    }
}
