package net.superricky.tpaplusplus.commands.accept;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Config;
import org.apache.commons.lang3.NotImplementedException;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

public class TPAAcceptCommand {

    public static void onRegisterCommandEvent(CommandDispatcher<CommandSourceStack> dispatcher) {
        dispatcher.register(literal(Config.TPAACCEPT_COMMAND_NAME.get())
                .executes(context -> acceptMostRecentTPA(context.getSource()))
                .then(argument("player", EntityArgument.player())
                        .executes(context -> acceptTPASpecified(context.getSource(), EntityArgument.getPlayer(context, "player")))));
    }

    private static int acceptMostRecentTPA(CommandSourceStack source) throws CommandSyntaxException, NotImplementedException {
        AcceptTPA.acceptTeleportRequest(source.getPlayerOrException());
        return 1;
    }

    private static int acceptTPASpecified(CommandSourceStack source, ServerPlayer sender) throws CommandSyntaxException, NotImplementedException {
        AcceptTPA.acceptTeleportRequest(source.getPlayerOrException(), sender);
        return 1;
    }

    private TPAAcceptCommand() {
    }
}
