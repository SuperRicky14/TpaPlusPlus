package net.superricky.tpaplusplus.config

import com.uchuhimo.konf.Config
import com.uchuhimo.konf.source.toml
import net.fabricmc.loader.api.FabricLoader
import net.superricky.tpaplusplus.Const
import net.superricky.tpaplusplus.config.command.*

object Config {
    private val config: Config = Config {
        addSpec(CommonSpec)
        addSpec(ColorSpec)
        addSpec(AdvancedSpec)
        addSpec(CommandEnableSpec)
        addSpec(CommandNameSpec)
        addSpec(CommandDelaySpec)
        addSpec(CommandCooldownSpec)
        addSpec(CommandDistanceSpec)
        addSpec(CommandLimitationsSpec)
    }
    .from.toml.resource(Const.CONFIG_FILE_NAME)
    .from.toml.watchFile(FabricLoader.getInstance().configDir.resolve(Const.CONFIG_FILE_PATH).toFile())
    .from.env()

    /**
     * @return Config instance
     */
    fun getConfig(): Config {
        return config
    }

    /**
     * Load and check config file.
     * Please call this function before use config.
     */
    fun loadAndVerifyConfig() {
        config.validateRequired()
    }
}
