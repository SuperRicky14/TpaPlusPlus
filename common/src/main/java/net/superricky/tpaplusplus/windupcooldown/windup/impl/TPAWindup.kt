package net.superricky.tpaplusplus.windupcooldown.windup.impl

import net.minecraft.server.level.ServerPlayer
import net.superricky.tpaplusplus.commands.send.SendTPA
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.windupcooldown.windup.AbstractWindup
import java.util.concurrent.atomic.AtomicInteger

class TPAWindup(
    val sender: ServerPlayer,
    val receiver: ServerPlayer
) : AbstractWindup() {

    override val windupPlayer: ServerPlayer = sender
    override val onWindupFinished: () -> Unit = { SendTPA.absoluteSendTeleportRequest(sender, receiver, false) }
    override val acceptX: Double = sender.x
    override val acceptY: Double = sender.y
    override val acceptZ: Double = sender.z
    override val windupDelay: AtomicInteger = AtomicInteger(Config.TPA_WINDUP.get().toInt())
    override val windupDistance: Double = Config.TPA_WINDUP_DISTANCE.get()
    override val commandName: String = Config.TPA_COMMAND_NAME.get()
}