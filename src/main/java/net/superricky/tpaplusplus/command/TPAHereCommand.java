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
public class TPAHereCommand {
    @SubscribeEvent
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal("tpahere")
                .then(argument("receiver", EntityArgument.player())
                        .executes(context -> {
                            return teleportPlayer(context.getSource(), EntityArgument.getPlayer(context, "receiver"));
                        })));
    }

    private static int teleportPlayer(CommandSourceStack source, ServerPlayer receiver) {
        TeleportHandler teleportHandler = TeleportHandler.getInstance();
        teleportHandler.sendTPARequestHere(source.getPlayer(), receiver);
        return 1;
    }
}