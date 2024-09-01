package net.superricky.tpaplusplus.commands.deny;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Config;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

public class TPADenyCommand {
    public static void onRegisterCommandEvent(CommandDispatcher<CommandSourceStack> dispatcher) {
        dispatcher.register(literal(Config.TPADENY_COMMAND_NAME.get())
                .executes(context -> denyMostRecentTPA(context.getSource()))
                .then(argument("player", EntityArgument.player())
                        .executes(context -> denyTPASpecified(context.getSource(), EntityArgument.getPlayer(context, "player")))));
    }

    private static int denyMostRecentTPA(CommandSourceStack source) throws CommandSyntaxException {
        DenyTPA.denyTeleportRequest(source.getPlayerOrException());
        return 1;
    }

    private static int denyTPASpecified(CommandSourceStack source, ServerPlayer sender) throws CommandSyntaxException {
        DenyTPA.denyTeleportRequest(source.getPlayerOrException(), sender);
        return 1;
    }

    private TPADenyCommand() {
    }
}
