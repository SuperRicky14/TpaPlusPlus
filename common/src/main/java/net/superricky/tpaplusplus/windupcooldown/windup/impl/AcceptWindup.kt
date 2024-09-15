package net.superricky.tpaplusplus.windupcooldown.windup.impl

import net.minecraft.server.level.ServerPlayer
import net.superricky.tpaplusplus.commands.accept.AcceptTPA
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.requests.Request
import net.superricky.tpaplusplus.windupcooldown.windup.AbstractWindup
import java.util.concurrent.atomic.AtomicInteger

class AcceptWindup(
    request: Request
) : AbstractWindup() {

    override val windupPlayer: ServerPlayer = request.receiver
    override val onWindupFinished: () -> Unit = { AcceptTPA.absoluteAcceptFunctionality(request, request.receiver) }
    override val acceptX: Double = request.receiver.x
    override val acceptY: Double = request.receiver.y
    override val acceptZ: Double = request.receiver.z
    override val windupDelay: AtomicInteger = AtomicInteger(Config.ACCEPT_WINDUP.get().toInt())
    override val windupDistance: Double = Config.ACCEPT_WINDUP_DISTANCE.get()
    override val commandName: String = Config.TPAACCEPT_COMMAND_NAME.get()
}