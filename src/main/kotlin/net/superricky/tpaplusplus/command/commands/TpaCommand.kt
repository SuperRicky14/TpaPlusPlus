package net.superricky.tpaplusplus.command.commands

import net.minecraft.server.command.CommandManager.literal
import net.superricky.tpaplusplus.command.BuildableCommand
import net.superricky.tpaplusplus.command.AsyncCommand
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.command.CommandDistanceSpec
import net.superricky.tpaplusplus.config.command.CommandNameSpec
import net.superricky.tpaplusplus.utility.LevelBoundVec3
import net.superricky.tpaplusplus.utility.LiteralNode
import net.superricky.tpaplusplus.utility.getDimension
import net.superricky.tpaplusplus.async.AsyncCommandData

object TpaCommand : BuildableCommand, AsyncCommand {
    override fun build(): LiteralNode =
        literal(Config.getConfig()[CommandNameSpec.tpaCommand])
            .build()

    override fun checkWindupDistance(asyncCommandData: AsyncCommandData): Boolean {
        if (Config.getConfig()[CommandDistanceSpec.tpaDistance] < 0) {
            return true
        }
        val originPos = asyncCommandData.getPos()
        val sender = asyncCommandData.getRequest().sender
        val nowPos = LevelBoundVec3(sender.getDimension(), sender.pos)
        val distance = originPos.distance(nowPos)
        if (distance == -1.0) {
            return false
        }
        return distance <= Config.getConfig()[CommandDistanceSpec.tpaDistance]
    }
}
