package net.superricky.tpaplusplus.utility

import net.minecraft.text.Style
import net.minecraft.text.TextColor
import net.superricky.tpaplusplus.config.config.ColorSpec
import net.superricky.tpaplusplus.config.config.Config
import kotlin.jvm.optionals.getOrNull

object TextColorPallet {
    private val config = Config.getConfig()
    val primary: Style
        get() = Style.EMPTY.withColor(TextColor.parse(config[ColorSpec.primary]).result().getOrNull())

    val primaryVariant: Style
        get() = Style.EMPTY.withColor(
            TextColor.parse(config[ColorSpec.primaryVariant]).result().getOrNull()
        )
    val secondary: Style
        get() = Style.EMPTY.withColor(
            TextColor.parse(config[ColorSpec.secondary]).result().getOrNull()
        )
    val secondaryVariant: Style
        get() = Style.EMPTY.withColor(
            TextColor.parse(config[ColorSpec.secondaryVariant]).result().getOrNull()
        )
    val error: Style get() = Style.EMPTY.withColor(TextColor.parse(config[ColorSpec.error]).result().getOrNull())
    val errorVariant: Style
        get() = Style.EMPTY.withColor(
            TextColor.parse(config[ColorSpec.errorVariant]).result().getOrNull()
        )
}
