package net.superricky.tpaplusplus.windupcooldown.windup

import net.minecraft.server.level.ServerPlayer
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger

/**
 * An abstract class where all other Windups derive from.
 * This allows for dynamic extension of Windups to support **any** command, without **any** modification to logic or
 * this base class.
 * Why inheritance and not composition?: I felt that windups as a feature likely won't ever change in a way that
 * requires custom logic, and this allows for minimal boilerplate when creating new Windups.
 */
abstract class AbstractWindup {
    val cancelled: AtomicBoolean = AtomicBoolean(false)

    abstract val windupPlayer: ServerPlayer
    abstract val onWindupFinished: () -> Unit
    abstract val acceptX: Double
    abstract val acceptY: Double
    abstract val acceptZ: Double
    abstract val windupDelay: AtomicInteger
    abstract val windupDistance: Double
    abstract val commandName: String
}