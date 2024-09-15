package net.superricky.tpaplusplus.windupcooldown.windup.impl

import net.minecraft.server.level.ServerPlayer
import net.superricky.tpaplusplus.commands.deny.DenyTPA
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.requests.Request
import net.superricky.tpaplusplus.windupcooldown.windup.AbstractWindup
import java.util.concurrent.atomic.AtomicInteger

class DenyWindup(
    request: Request
) : AbstractWindup() {

    override val windupPlayer: ServerPlayer = request.receiver
    override val onWindupFinished: () -> Unit = { DenyTPA.absoluteDeny(request) }
    override val acceptX: Double = request.receiver.x
    override val acceptY: Double = request.receiver.y
    override val acceptZ: Double = request.receiver.z
    override val windupDelay: AtomicInteger = AtomicInteger(Config.DENY_WINDUP.get().toInt())
    override val windupDistance: Double = Config.DENY_WINDUP_DISTANCE.get()
    override val commandName: String = Config.TPADENY_COMMAND_NAME.get()
}