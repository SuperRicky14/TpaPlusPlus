package net.superricky.tpaplusplus.utility

import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.text.Text
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.command.CommandLimitationsSpec
import kotlin.math.absoluteValue

object LimitationHelper {

    private fun checkDimensionLimitation(sender: ServerPlayerEntity, receiver: ServerPlayerEntity): Boolean {
        return Config.getConfig()[CommandLimitationsSpec.crossDimAllowed] ||
                (sender.world.dimension == receiver.world.dimension)
    }

    fun checkDimensionLimitation(sender: ServerPlayerEntity, target: ServerDimension): Boolean {
        return Config.getConfig()[CommandLimitationsSpec.crossDimAllowed] ||
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
            Config.getConfig()[CommandLimitationsSpec.crossDimAllowed] &&
            Config.getConfig()[CommandLimitationsSpec.ignoreDistanceCrossDim]
        ) {
            return 0.0
        }
        if (Config.getConfig()[CommandLimitationsSpec.maxTpDistance] <= 0.0 ||
            Config.getConfig()[CommandLimitationsSpec.minTpDistance] <= 0.0
        ) {
            return 0.0
        }
        val senderPos = LevelBoundVec3(sender.getDimension(), sender.pos)
        val receiverPos = LevelBoundVec3(receiver.getDimension(), receiver.pos)
        val distance = senderPos.distance(receiverPos)
        if (distance > Config.getConfig()[CommandLimitationsSpec.maxTpDistance]) {
            return distance.absoluteValue
        }
        if (distance < Config.getConfig()[CommandLimitationsSpec.minTpDistance]) {
            return -distance.absoluteValue
        }
        return 0.0
    }

    fun checkLimitation(sender: ServerPlayerEntity, receiver: ServerPlayerEntity): Text? {
        val crossDimension = checkDimensionLimitation(sender, receiver)
        val distance = checkDistanceLimitation(sender, receiver)
        if (!crossDimension) {
            return Text.translatable(
                "command.error.cross_dim",
                sender.getDimension().value.toString().literal().setStyle(TextColorPallet.errorVariant),
                receiver.getDimension().value.toString().literal().setStyle(TextColorPallet.errorVariant)
            ).setStyle(TextColorPallet.error)
        }
        if (distance == 0.0) {
            return null
        }
        if (distance > 0) {
            return Text.translatable(
                "command.error.distance.too_far",
                Config.getConfig()[CommandLimitationsSpec.maxTpDistance].toString().literal()
                    .setStyle(TextColorPallet.errorVariant),
                String.format("%.2f", distance).literal().setStyle(TextColorPallet.errorVariant)
            ).setStyle(TextColorPallet.error)
        }
        return Text.translatable(
            "command.error.distance.too_close",
            Config.getConfig()[CommandLimitationsSpec.minTpDistance].toString().literal()
                .setStyle(TextColorPallet.errorVariant),
            String.format("%.2f", distance.absoluteValue).literal().setStyle(TextColorPallet.errorVariant)
        ).setStyle(TextColorPallet.error)
    }
}
