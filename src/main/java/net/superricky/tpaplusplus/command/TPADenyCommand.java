package net.superricky.tpaplusplus.command;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.superricky.tpaplusplus.utils.RequestAction;
import net.superricky.tpaplusplus.utils.SenderReceiverPair;
import net.superricky.tpaplusplus.utils.TeleportHandler;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

@Mod.EventBusSubscriber
public class TPADenyCommand {
    @SubscribeEvent
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal("tpadeny")
                .executes(context -> {
                    return denyMostRecentTPA(context.getSource());
                })
                .then(argument("receiver", EntityArgument.player())
                        .executes(context -> {
                            return denyTPASpecified(context.getSource() ,EntityArgument.getPlayer(context, "receiver"));
                        })));
    }
    private static int denyMostRecentTPA(CommandSourceStack source) throws CommandSyntaxException {
        TeleportHandler teleportHandler = TeleportHandler.getInstance();
        teleportHandler.handleMostRecentTPARequest(source.getPlayerOrException(), RequestAction.CANCEL);
        return 1;
    }

    private static int denyTPASpecified(CommandSourceStack source, ServerPlayer receiver) throws CommandSyntaxException {
        TeleportHandler teleportHandler = TeleportHandler.getInstance();
        teleportHandler.handleTPARequest(new SenderReceiverPair(source.getPlayerOrException(), receiver), RequestAction.CANCEL);
        return 1;
    }
}
