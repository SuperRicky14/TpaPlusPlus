package net.superricky.tpaplusplus.utility

import net.minecraft.util.math.Position
import net.minecraft.util.math.Vec3d
import net.minecraft.world.World
import net.superricky.tpaplusplus.GlobalConst.NETHER_COEFFICIENT

class LevelBoundVec3(
    val serverLevel: ServerDimension,
    pX: Double,
    pY: Double,
    pZ: Double,
    val yaw: Float = 0.0f,
    val pitch: Float = 0.0f
) : Vec3d(pX, pY, pZ) {

    constructor(serverLevel: ServerDimension, pos: Position) : this(serverLevel, pos.x, pos.y, pos.z)
    constructor(serverLevel: ServerDimension, pos: Position, yaw: Float, pitch: Float) : this(
        serverLevel,
        pos.x,
        pos.y,
        pos.z,
        yaw,
        pitch
    )

    fun distance(other: LevelBoundVec3): Double {
        if (serverLevel != other.serverLevel) {
            if (serverLevel == World.NETHER && other.serverLevel != World.NETHER) {
                return other.distanceTo(
                    Vec3d(
                        x * NETHER_COEFFICIENT,
                        y,
                        z * NETHER_COEFFICIENT
                    )
                )
            }
            if (other.serverLevel == World.NETHER && serverLevel != World.NETHER) {
                return distanceTo(
                    Vec3d(
                        other.x * NETHER_COEFFICIENT,
                        other.y,
                        other.z * NETHER_COEFFICIENT
                    )
                )
            }
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
