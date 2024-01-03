package net.superricky.tpaplusplus.command;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.superricky.tpaplusplus.util.manager.RequestManager;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

@Mod.EventBusSubscriber
public class TPACancelCommand {
    @SubscribeEvent
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal("tpacancel")
                .executes(context -> cancelMostRecentTPA(context.getSource()))
                .then(argument("player", EntityArgument.player())
                        .executes(context -> cancelTPASpecified(context.getSource(), EntityArgument.getPlayer(context, "player")))));
    }

    private static int cancelMostRecentTPA(CommandSourceStack source) throws CommandSyntaxException {
        RequestManager.cancelTeleportRequest(source.getPlayerOrException());
        return 1;
    }

    private static int cancelTPASpecified(CommandSourceStack source, ServerPlayer receiver) throws CommandSyntaxException {
        RequestManager.cancelTeleportRequest(source.getPlayerOrException(), receiver);
        return 1;
    }

    private TPACancelCommand() {
    }
}
