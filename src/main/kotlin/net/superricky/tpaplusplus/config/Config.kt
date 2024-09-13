package net.superricky.tpaplusplus.config

import com.uchuhimo.konf.Config
import com.uchuhimo.konf.RequiredItem
import com.uchuhimo.konf.source.toml
import net.fabricmc.loader.api.FabricLoader
import net.superricky.tpaplusplus.GlobalConst
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
        .from.toml.resource(GlobalConst.CONFIG_FILE_NAME)
        .from.toml.watchFile(FabricLoader.getInstance().configDir.resolve(GlobalConst.CONFIG_FILE_PATH).toFile())
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
        @Suppress("UNCHECKED_CAST")
        // CommandNameSpec are all RequiredItem<String> type, so this cast is safe
        CommandNameSpec.items.forEach { replaceCommand(it as? RequiredItem<String> ?: return@forEach) }
    }

    /**
     * Replace / before command
     */
    private fun replaceCommand(item: RequiredItem<String>) {
        if (config[item].startsWith("/")) {
            config[item] = config[item].replace("/", "")
        }
    }
}
