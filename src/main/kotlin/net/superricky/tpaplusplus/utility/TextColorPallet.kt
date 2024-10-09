package net.superricky.tpaplusplus.utility

import net.minecraft.text.Style
import net.minecraft.text.TextColor
import net.superricky.tpaplusplus.config.config.ColorSpec
import net.superricky.tpaplusplus.config.config.Config.get
import kotlin.jvm.optionals.getOrNull

object TextColorPallet {
    val primary: Style
        get() = Style.EMPTY.withColor(TextColor.parse(ColorSpec.primary.get()).result().getOrNull())

    val primaryVariant: Style
        get() = Style.EMPTY.withColor(
            TextColor.parse(ColorSpec.primaryVariant.get()).result().getOrNull()
        )
    val secondary: Style
        get() = Style.EMPTY.withColor(
            TextColor.parse(ColorSpec.secondary.get()).result().getOrNull()
        )
    val secondaryVariant: Style
        get() = Style.EMPTY.withColor(
            TextColor.parse(ColorSpec.secondaryVariant.get()).result().getOrNull()
        )
    val error: Style get() = Style.EMPTY.withColor(TextColor.parse(ColorSpec.error.get()).result().getOrNull())
    val errorVariant: Style
        get() = Style.EMPTY.withColor(
            TextColor.parse(ColorSpec.errorVariant.get()).result().getOrNull()
        )
}
