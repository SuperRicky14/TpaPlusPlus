package net.superricky.tpaplusplus.limitations;

import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.TPAPlusPlus;
import net.superricky.tpaplusplus.config.Config;
import net.superricky.tpaplusplus.config.Messages;
import net.superricky.tpaplusplus.config.formatters.MessageParser;
import org.jetbrains.annotations.Nullable;

import java.util.Map;
import java.util.Objects;

public class LimitationManager {
    private LimitationManager() {
    }

    private static final String DISTANCE_IDENTIFIER = "distance";
    private static final String EXPECTED_DISTANCE_IDENTIFIER = "expectedDistance";
    private static final String OTHER_PLAYER_NAME_IDENTIFIER = "otherPlayerName";
    private static final String EXECUTOR_DIMENSION_IDENTIFIER = "executorDimension";
    private static final String OTHER_PLAYER_DIMENSION_IDENTIFIER = "otherPlayerDimension";


    @Nullable
    public static Limitation getLimitationType(ServerPlayer sender, ServerPlayer receiver) {

        // PLAYERS ARE NOT IN THE SAME DIMENSION
        if (Boolean.FALSE.equals(sender.serverLevel().dimension().location().getPath().equals(receiver.serverLevel().dimension().location().getPath()))) {
            // If the configuration prevents players from teleporting interdimensional
            if (Boolean.FALSE.equals(Config.ALLOW_INTER_DIMENSIONAL_TELEPORT.get()))
                return new Limitation(LimitationType.DIFFERENT_DIMENSIONS, sender.serverLevel(), receiver.serverLevel());

            // If the configuration disables range checks interdimensional
            if (Boolean.TRUE.equals(Config.DISABLE_RANGE_CHECKS_INTER_DIMENSIONAL.get()) && sender.serverLevel().dimensionTypeId().equals(receiver.serverLevel().dimensionTypeId())) {
                return null;
            }
        }

        // Get the sender's x,y,z coordinates
        double senderX = sender.getX();
        double senderY = sender.getY();
        double senderZ = sender.getZ();

        // Get the receiver's x,y,z coordinates'
        double receiverX = receiver.getX();
        double receiverY = receiver.getY();
        double receiverZ = receiver.getZ();

        // Assign the distance between the players to a double to avoid calculating the distance 4 times.
        double distanceBetweenPlayers = TPAPlusPlus.distance3D(senderX, senderY, senderZ, receiverX, receiverY, receiverZ);

        // The players are further away than the specified distance in the config, and far away checks are enabled.
        if (Config.FURTHEST_ALLOWED_DISTANCE.get() != 0 && distanceBetweenPlayers > Config.FURTHEST_ALLOWED_DISTANCE.get())
            return new Limitation(LimitationType.TOO_FAR, distanceBetweenPlayers);

        // The players are closer than the specified distance in the config, and far away checks are enabled.
        if (Config.CLOSEST_ALLOWED_DISTANCE.get() != 0 && distanceBetweenPlayers < Config.CLOSEST_ALLOWED_DISTANCE.get())
            return new Limitation(LimitationType.TOO_CLOSE, distanceBetweenPlayers);

        return null;
    }

    /**
     * Notifies the executor and stops them from being able to send a teleport request
     *
     * @param executor    The player sending the teleport request
     * @param otherPlayer The player receiving the teleport request
     * @return TRUE if the request passed all limitation checks | FALSE if the request failed any limitation checks
     */
    public static boolean notifyAndCheckAllowedToTeleport(ServerPlayer executor, ServerPlayer otherPlayer, boolean notifyBothPlayers) {
        Limitation limitation = getLimitationType(executor, otherPlayer);

        // If the Limitation is null, then there is no Limitation to be applied, so we return true here.
        if (Objects.isNull(limitation)) return true;

        switch (limitation.type()) {
            case TOO_FAR -> {
                executor.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.ERR_TOO_FAR_EXECUTOR.get(),
                        Map.of(DISTANCE_IDENTIFIER, Math.round(limitation.distance()),
                                EXPECTED_DISTANCE_IDENTIFIER, Math.round(Config.FURTHEST_ALLOWED_DISTANCE.get())))));

                if (notifyBothPlayers)
                    executor.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.ERR_TOO_FAR_OTHER_PLAYER.get(),
                            Map.of(DISTANCE_IDENTIFIER, Math.round(limitation.distance()),
                                    EXPECTED_DISTANCE_IDENTIFIER, Math.round(Config.CLOSEST_ALLOWED_DISTANCE.get()),
                                    OTHER_PLAYER_NAME_IDENTIFIER, executor.getName().getString()))));
                return false;
            }

            case TOO_CLOSE -> {
                executor.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.ERR_TOO_CLOSE_EXECUTOR.get(),
                        Map.of(DISTANCE_IDENTIFIER, Math.round(limitation.distance()),
                                EXPECTED_DISTANCE_IDENTIFIER, Math.round(Config.CLOSEST_ALLOWED_DISTANCE.get())))));

                if (notifyBothPlayers)
                    otherPlayer.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.ERR_TOO_CLOSE_OTHER_PLAYER.get(),
                            Map.of(DISTANCE_IDENTIFIER, Math.round(limitation.distance()),
                                    EXPECTED_DISTANCE_IDENTIFIER, Math.round(Config.CLOSEST_ALLOWED_DISTANCE.get()),
                                    OTHER_PLAYER_NAME_IDENTIFIER, executor.getName().getString()))));
                return false;
            }

            case DIFFERENT_DIMENSIONS -> {
                executor.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.ERR_DIFFERENT_DIMENSIONS_EXECUTOR.get(),
                        Map.of(EXECUTOR_DIMENSION_IDENTIFIER, limitation.executorLevel().dimension().location().getPath(),
                                OTHER_PLAYER_DIMENSION_IDENTIFIER, limitation.otherPlayerLevel().dimension().location().getPath()))));

                if (notifyBothPlayers)
                    otherPlayer.sendSystemMessage(Component.literal(MessageParser.enhancedFormatter(Messages.ERR_DIFFERENT_DIMENSIONS_OTHER_PLAYER.get(),
                            Map.of(EXECUTOR_DIMENSION_IDENTIFIER, limitation.otherPlayerLevel().dimension().location().getPath(),
                                    OTHER_PLAYER_DIMENSION_IDENTIFIER, limitation.executorLevel().dimension().location().getPath(),
                                    OTHER_PLAYER_NAME_IDENTIFIER, executor.getName().getString()))));
                return false;
            }
        }

        // No known value.
        return false;
    }
}
