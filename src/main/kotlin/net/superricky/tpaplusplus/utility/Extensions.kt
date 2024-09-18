package net.superricky.tpaplusplus.utility

import kotlinx.coroutines.launch
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.registry.RegistryKey
import net.minecraft.registry.RegistryKeys
import net.minecraft.text.MutableText
import net.minecraft.text.Style
import net.minecraft.text.Text
import net.minecraft.util.Identifier
import net.superricky.tpaplusplus.TpaPlusPlus
import net.superricky.tpaplusplus.config.AdvancedSpec
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.database.DatabaseManager

fun String.literal(): MutableText = Text.literal(this)
fun String.translate(): MutableText = Text.translatable(this)
fun String.getWorld(): ServerDimension =
    RegistryKey.of(RegistryKeys.WORLD, Identifier.of(this))

fun PlayerEntity.toggleOn() = TpaPlusPlus.launch {
    DatabaseManager.playerSwitchBlock(uuid, true)
}

fun PlayerEntity.toggleOff() = TpaPlusPlus.launch {
    DatabaseManager.playerSwitchBlock(uuid, false)
}

fun PlayerEntity.getColoredName(color: Style): MutableText? = this.name.literalString?.literal()?.setStyle(color)
fun PlayerEntity.getDimension(): ServerDimension = this.world.registryKey
fun PlayerEntity.sendMessage(
    translateKey: String,
    player: PlayerEntity,
    outStyle: Style = TextColorPallet.primary,
    inStyle: Style = TextColorPallet.secondary
) {
    this.sendMessage(
        Text.translatable(
            translateKey,
            player.getColoredName(inStyle)
        ).setStyle(outStyle)
    )
}

@Suppress("MagicNumber")
fun Long.translateSecondToTick(): Long =
    if (Config.getConfig()[AdvancedSpec.unblockingTickLoop]) {
        this * Config.getConfig()[AdvancedSpec.asyncLoopRate]
    } else {
        this * 20
    }

@Suppress("MagicNumber")
fun Double.translateTickToSecond(): Double =
    if (Config.getConfig()[AdvancedSpec.unblockingTickLoop]) {
        this / Config.getConfig()[AdvancedSpec.asyncLoopRate].toDouble()
    } else {
        this / 20.0
    }
