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
public class TPAAcceptCommand {
    @SubscribeEvent
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal("tpaaccept")
                        .executes(context -> {
                            return acceptMostRecentTPA(context.getSource());
                        })
                .then(argument("receiver", EntityArgument.player())
                        .executes(context -> {
                            return acceptTPASpecified(EntityArgument.getPlayer(context, "receiver"));
                        })));
    }
    private static int acceptMostRecentTPA(CommandSourceStack source) {
        TeleportHandler teleportHandler = TeleportHandler.getInstance();
        teleportHandler.acceptMostRecentTPARequest(source.getPlayer());
        return 1;
    }

    private static int acceptTPASpecified(ServerPlayer receiver) {
        TeleportHandler teleportHandler = TeleportHandler.getInstance();
        teleportHandler.acceptSpecifiedTPARequest(receiver);
        return 1;
    }
}
