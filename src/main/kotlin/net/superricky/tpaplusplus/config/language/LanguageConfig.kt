package net.superricky.tpaplusplus.config.language

import com.uchuhimo.konf.Config
import com.uchuhimo.konf.RequiredItem
import com.uchuhimo.konf.source.Source
import com.uchuhimo.konf.source.toml
import dev.architectury.platform.Platform
import net.minecraft.text.MutableText
import net.minecraft.text.TranslatableTextContent
import net.superricky.tpaplusplus.GlobalConst.DEFAULT_LANG_FILE_NAME
import net.superricky.tpaplusplus.GlobalConst.DEFAULT_LANG_FILE_SOURCE_PATH
import net.superricky.tpaplusplus.GlobalConst.LANG_FOLDER_NAME
import net.superricky.tpaplusplus.GlobalConst.LANG_FOLDER_PATH
import net.superricky.tpaplusplus.GlobalConst.MOD_ID
import net.superricky.tpaplusplus.GlobalConst.logger
import net.superricky.tpaplusplus.config.language.command.*
import net.superricky.tpaplusplus.config.language.error.ErrorRequestSpec
import net.superricky.tpaplusplus.config.language.error.ErrorSpec
import java.nio.file.Files
import kotlin.io.path.Path

object LanguageConfig {
    private lateinit var config: Config
    private val supportedLanguage = listOf("en_us", "zh_cn", "zh_tw")
    private lateinit var languageFileName: String

    fun loadLangFile(language: String) {
        languageFileName = "$language.toml"
        if (!checkLanguageFile(language)) {
            languageFileName = DEFAULT_LANG_FILE_NAME
        }
        config = Config {
            addSpec(SystemSpec)
            addSpec(ErrorSpec)
            addSpec(ErrorRequestSpec)
            addSpec(WindupSpec)
            addSpec(TeleportSpec)
            addSpec(CooldownSpec)
            addSpec(ToggleSpec)
            addSpec(BlockSpec)
            addSpec(UnblockSpec)
            addSpec(BackSpec)
            addSpec(TpaSpec)
            addSpec(TpaHereSpec)
        }
            .from.toml.resource(DEFAULT_LANG_FILE_SOURCE_PATH)
            .from.toml.watchFile(
                Platform.getConfigFolder().resolve(LANG_FOLDER_PATH).resolve(languageFileName).toFile()
            )
            .from.env()
        config.validateRequired()
    }

    fun addLoadListener(listener: Function1<Source, Unit>) {
        config.beforeLoad { listener(it) }
    }

    private fun checkLanguageFile(language: String): Boolean {
        val languageFilePath = Path(LANG_FOLDER_PATH).resolve(languageFileName)
        val languageSourcePath = Path(LANG_FOLDER_NAME).resolve(languageFileName).toString()
        if (!Files.exists(Platform.getConfigFolder().resolve(languageFilePath))) {
            // If language has been supported
            if (supportedLanguage.contains(language)) {
                logger.info("No $languageFileName file, Creating")
                try {
                    Files.copy(
                        Platform.getMod(MOD_ID).findResource(languageSourcePath).get(),
                        Platform.getConfigFolder().resolve(languageFilePath)
                    )
                } catch (e: Exception) {
                    logger.error(e)
                    return false
                }
                return true
            }
            logger.info("No $languageFileName file found, use default language")
            return false
        }
        return true
    }

    fun RequiredItem<String>.getMutableText(vararg args: Any?): MutableText {
        return MutableText.of(TranslatableTextContent("", config[this], args))
    }
}
