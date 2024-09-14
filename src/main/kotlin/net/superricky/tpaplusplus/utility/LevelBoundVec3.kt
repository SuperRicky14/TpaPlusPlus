package net.superricky.tpaplusplus.utility

import net.minecraft.util.math.Position
import net.minecraft.util.math.Vec3d

class LevelBoundVec3(
    val serverLevel: ServerDimension,
    pX: Double,
    pY: Double,
    pZ: Double
) : Vec3d(pX, pY, pZ) {

    constructor(serverLevel: ServerDimension, pos: Position) : this(serverLevel, pos.x, pos.y, pos.z)

    fun distance(other: LevelBoundVec3): Double {
        if (serverLevel != other.serverLevel) {
            return -1.0
        }
        return distanceTo(other)
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true // self-equality

        if (other !is LevelBoundVec3) return false // ensure object is LevelBoundVec3

        // Compare every field for equality:
        return x.compareTo(other.x) == 0 && y.compareTo(other.y) == 0 &&
                z.compareTo(other.z) == 0 && serverLevel == other.serverLevel
    }

    override fun hashCode(): Int {
        var result = super.hashCode()
        result = 31 * result + serverLevel.hashCode()
        return result
    }
}
