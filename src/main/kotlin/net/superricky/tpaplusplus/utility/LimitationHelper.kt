package net.superricky.tpaplusplus.utility

import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.text.Text
import net.superricky.tpaplusplus.config.config.Config.get
import net.superricky.tpaplusplus.config.config.command.CommandLimitationsSpec
import net.superricky.tpaplusplus.config.language.LanguageConfig.getMutableText
import net.superricky.tpaplusplus.config.language.error.ErrorSpec
import kotlin.math.absoluteValue

object LimitationHelper {

    private fun checkDimensionLimitation(sender: ServerPlayerEntity, receiver: ServerPlayerEntity): Boolean {
        return CommandLimitationsSpec.crossDimAllowed.get() ||
                (sender.world.dimension == receiver.world.dimension)
    }

    fun checkDimensionLimitation(sender: ServerPlayerEntity, target: ServerDimension): Boolean {
        return CommandLimitationsSpec.crossDimAllowed.get() ||
                (sender.getDimension() == target)
    }

    /**
     * Check teleport distance limitation
     * @return a double, 0.0 if legal,
     * > 0 if over max limitation,
     * < 0 if below min limitation,
     * return absolute value is the distance value
     */
    private fun checkDistanceLimitation(
        sender: ServerPlayerEntity,
        receiver: ServerPlayerEntity
    ): Double {
        if (sender.world.dimension != receiver.world.dimension &&
            CommandLimitationsSpec.crossDimAllowed.get() &&
            CommandLimitationsSpec.ignoreDistanceCrossDim.get()
        ) {
            return 0.0
        }
        val senderPos = LevelBoundVec3(sender.getDimension(), sender.pos)
        val receiverPos = LevelBoundVec3(receiver.getDimension(), receiver.pos)
        val distance = senderPos.distance(receiverPos)
        if (CommandLimitationsSpec.maxTpDistance.get() > 0.0 &&
            distance > CommandLimitationsSpec.maxTpDistance.get()
        ) {
            return distance.absoluteValue
        }
        if (CommandLimitationsSpec.minTpDistance.get() > 0.0 &&
            distance < CommandLimitationsSpec.minTpDistance.get()
        ) {
            return -distance.absoluteValue
        }
        return 0.0
    }

    fun checkLimitation(sender: ServerPlayerEntity, receiver: ServerPlayerEntity): Text? {
        val crossDimension = checkDimensionLimitation(sender, receiver)
        val distance = checkDistanceLimitation(sender, receiver)
        if (!crossDimension) {
            return ErrorSpec.crossDim.getMutableText(
                sender.getDimension().value.toString().literal().setStyle(TextColorPallet.errorVariant),
                receiver.getDimension().value.toString().literal().setStyle(TextColorPallet.errorVariant)
            ).setStyle(TextColorPallet.error)
        }
        if (distance == 0.0) {
            return null
        }
        if (distance > 0) {
            return ErrorSpec.tooFar.getMutableText(
                CommandLimitationsSpec.maxTpDistance.get().toString().literal()
                    .setStyle(TextColorPallet.errorVariant),
                String.format("%.2f", distance).literal().setStyle(TextColorPallet.errorVariant)
            ).setStyle(TextColorPallet.error)
        }
        return ErrorSpec.tooClose.getMutableText(
            CommandLimitationsSpec.minTpDistance.get().toString().literal()
                .setStyle(TextColorPallet.errorVariant),
            String.format("%.2f", distance.absoluteValue).literal().setStyle(TextColorPallet.errorVariant)
        ).setStyle(TextColorPallet.error)
    }
}
