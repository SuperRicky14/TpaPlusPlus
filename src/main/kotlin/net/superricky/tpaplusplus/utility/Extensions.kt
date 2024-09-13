package net.superricky.tpaplusplus.utility

import kotlinx.coroutines.launch
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.text.MutableText
import net.minecraft.text.Style
import net.minecraft.text.Text
import net.superricky.tpaplusplus.TpaPlusPlus
import net.superricky.tpaplusplus.database.DatabaseManager

fun String.literal(): MutableText = Text.literal(this)
fun String.translate(): MutableText = Text.translatable(this)

fun PlayerEntity.toggleOn() = TpaPlusPlus.launch {
    DatabaseManager.playerSwitchBlock(uuid, true)
}

fun PlayerEntity.toggleOff() = TpaPlusPlus.launch {
    DatabaseManager.playerSwitchBlock(uuid, false)
}

fun PlayerEntity.getColoredName(color: Style): MutableText? = this.name.literalString?.literal()?.setStyle(color)
