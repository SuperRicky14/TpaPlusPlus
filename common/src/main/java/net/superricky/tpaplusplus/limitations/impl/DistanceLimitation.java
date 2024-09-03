package net.superricky.tpaplusplus.limitations.impl;

import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.config.formatters.MessageParser;
import net.superricky.tpaplusplus.limitations.Limitation;

import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.TPAPlusPlus;
import net.superricky.tpaplusplus.config.Config;

import java.util.Map;

public class DistanceLimitation implements Limitation {
    private static final String DISTANCE_IDENTIFIER = "distance";
    private static final String EXPECTED_DISTANCE_IDENTIFIER = "expectedDistance";

    @Override
    public boolean isViolated(ServerPlayer sender, ServerPlayer receiver) {
        double distance = TPAPlusPlus.distance3D(
                sender.getX(), sender.getY(), sender.getZ(),
                receiver.getX(), receiver.getY(), receiver.getZ());

        return (Config.FURTHEST_ALLOWED_DISTANCE.get() != 0 && distance > Config.FURTHEST_ALLOWED_DISTANCE.get())
                || (Config.CLOSEST_ALLOWED_DISTANCE.get() != 0 && distance < Config.CLOSEST_ALLOWED_DISTANCE.get());
    }

    @Override
    public String getViolationMessage(ServerPlayer sender, ServerPlayer receiver) {
        double distance = TPAPlusPlus.distance3D(
                sender.getX(), sender.getY(), sender.getZ(),
                receiver.getX(), receiver.getY(), receiver.getZ());

        if (distance > Config.FURTHEST_ALLOWED_DISTANCE.get()) {
            return MessageParser.enhancedFormatter(Messages.ERR_TOO_FAR_EXECUTOR.get(),
                    Map.of(DISTANCE_IDENTIFIER, Math.round(distance),
                            EXPECTED_DISTANCE_IDENTIFIER, Math.round(Config.FURTHEST_ALLOWED_DISTANCE.get())));
        } else {
            return MessageParser.enhancedFormatter(Messages.ERR_TOO_CLOSE_EXECUTOR.get(),
                    Map.of(DISTANCE_IDENTIFIER, Math.round(distance),
                            EXPECTED_DISTANCE_IDENTIFIER, Math.round(Config.CLOSEST_ALLOWED_DISTANCE.get())));
        }
    }
}
