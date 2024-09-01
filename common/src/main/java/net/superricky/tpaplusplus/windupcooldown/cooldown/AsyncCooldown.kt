package net.superricky.tpaplusplus.windupcooldown.cooldown

import kotlinx.coroutines.*
import net.superricky.tpaplusplus.TPAPlusPlus
import net.superricky.tpaplusplus.windupcooldown.CommandType
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.util.*

private val LOGGER: Logger = LoggerFactory.getLogger(TPAPlusPlus.MOD_ID)
private val dispatcher: CoroutineDispatcher = Dispatchers.IO
private val scope: CoroutineScope = CoroutineScope(dispatcher)

fun scheduleCooldown(playerUUID: UUID, delay: Int, type: CommandType) = scope.launch {
    val cooldownData = CooldownData(playerUUID, delay, type)

    AsyncCooldownHelper.getCooldownSet().add(cooldownData)

    cooldownLogic(cooldownData)
}

private suspend fun cooldownLogic(cooldownData: CooldownData) = coroutineScope {
    withContext(dispatcher) {
        while (cooldownData.getCooldownDelay() > 0) {
            delay(1000)
            cooldownData.setCooldownDelay(cooldownData.getCooldownDelay() - 1)
        }
    }

    AsyncCooldownHelper.getCooldownSet().remove(cooldownData)
}
