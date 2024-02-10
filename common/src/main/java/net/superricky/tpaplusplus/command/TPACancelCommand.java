package net.superricky.tpaplusplus.command;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandBuildContext;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.util.configuration.Config;
import net.superricky.tpaplusplus.util.manager.RequestManager;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

public class TPACancelCommand {
    public static void onRegisterCommandEvent(CommandDispatcher<CommandSourceStack> dispatcher, CommandBuildContext registry, Commands.CommandSelection selection) {
        dispatcher.register(literal(Config.TPACANCEL_COMMAND_NAME.get())
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
