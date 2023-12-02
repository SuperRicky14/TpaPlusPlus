package net.superricky.tpaplusplus.command;

import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.superricky.tpaplusplus.utils.TeleportHandler;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

@Mod.EventBusSubscriber
public class TPACancelCommand {
    @SubscribeEvent
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal("tpacancel")
                .executes(context -> {
                    return cancelMostRecentTPA(context.getSource());
                })
                .then(argument("receiver", EntityArgument.player())
                        .executes(context -> {
                            return cancelTPASpecified(EntityArgument.getPlayer(context, "receiver"));
                        })));
    }
    private static int cancelMostRecentTPA(CommandSourceStack source) {
        TeleportHandler teleportHandler = TeleportHandler.getInstance();
        teleportHandler.cancelMostRecentTPARequest(source.getPlayer());
        return 1;
    }

    private static int cancelTPASpecified(ServerPlayer receiver) {
        TeleportHandler teleportHandler = TeleportHandler.getInstance();
        teleportHandler.cancelSpecifiedTPARequest(receiver);
        return 1;
    }
}
