package net.superricky.tpaplusplus.commands.back

import net.minecraft.server.level.ServerLevel
import net.minecraft.world.phys.Vec3

data class LevelBoundVec3(val serverLevel: ServerLevel, val pX: Double, val pY: Double, val pZ: Double) : Vec3(pX, pY, pZ)
