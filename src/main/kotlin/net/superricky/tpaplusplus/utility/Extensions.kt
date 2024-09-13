package net.superricky.tpaplusplus.utility

import net.minecraft.text.MutableText
import net.minecraft.text.Text

fun String.literal(): MutableText = Text.literal(this)
fun String.translate(): MutableText = Text.translatable(this)
