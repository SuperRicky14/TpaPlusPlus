package net.superricky.tpaplusplus

import net.fabricmc.api.ModInitializer
import org.apache.logging.log4j.LogManager

/**
 * Main class
 */
class TpaPlusPlus : ModInitializer {
    private val logger = LogManager.getLogger("TpaPlusPlus")

    override fun onInitialize() {
        logger.info("TPA plus started")
    }
}
