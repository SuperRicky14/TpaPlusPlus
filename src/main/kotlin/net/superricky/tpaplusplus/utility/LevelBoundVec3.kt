package net.superricky.tpaplusplus.utility

import net.minecraft.util.math.Position
import net.minecraft.util.math.Vec3d
import net.minecraft.world.World
import net.superricky.tpaplusplus.GlobalConst.NETHER_COEFFICIENT
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.command.CommandLimitationsSpec

class LevelBoundVec3(
    val serverLevel: ServerDimension,
    pX: Double,
    pY: Double,
    pZ: Double
) : Vec3d(pX, pY, pZ) {

    constructor(serverLevel: ServerDimension, pos: Position) : this(serverLevel, pos.x, pos.y, pos.z)

    fun distance(other: LevelBoundVec3): Double {
        if (serverLevel != other.serverLevel) {
            if (!Config.getConfig()[CommandLimitationsSpec.crossDimAllowed]) {
                return -1.0
            }
            if (Config.getConfig()[CommandLimitationsSpec.ignoreDistanceCrossDim]) {
                return 0.0
            }
            if (serverLevel == World.NETHER && other.serverLevel != World.NETHER) {
                return other.distanceTo(
                    Vec3d(
                        x * NETHER_COEFFICIENT,
                        y * NETHER_COEFFICIENT,
                        z * NETHER_COEFFICIENT
                    )
                )
            }
            if (other.serverLevel == World.NETHER && serverLevel != World.NETHER) {
                return distanceTo(
                    Vec3d(
                        other.x * NETHER_COEFFICIENT,
                        other.y * NETHER_COEFFICIENT,
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
