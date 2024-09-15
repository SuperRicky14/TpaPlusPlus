package net.superricky.tpaplusplus.windupcooldown.windup.impl

import net.minecraft.server.level.ServerPlayer
import net.superricky.tpaplusplus.commands.cancel.CancelTPA
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.requests.Request
import net.superricky.tpaplusplus.windupcooldown.windup.AbstractWindup
import java.util.concurrent.atomic.AtomicInteger

class CancelWindup(
    request: Request
) : AbstractWindup() {

    override val windupPlayer: ServerPlayer = request.sender
    override val onWindupFinished: () -> Unit = { CancelTPA.absoluteCancel(request) }
    override val acceptX: Double = request.sender.x
    override val acceptY: Double = request.sender.y
    override val acceptZ: Double = request.sender.z
    override val windupDelay: AtomicInteger = AtomicInteger(Config.CANCEL_WINDUP.get().toInt())
    override val windupDistance: Double = Config.CANCEL_WINDUP_DISTANCE.get()
    override val commandName: String = Config.TPACANCEL_COMMAND_NAME.get()
}

