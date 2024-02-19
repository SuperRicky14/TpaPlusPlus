package net.superricky.tpaplusplus.commands.unblock;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Config;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

public class TPUnBlockCommand {
    private TPUnBlockCommand() {
    }

    private static int unBlockPlayer(CommandSourceStack source, ServerPlayer blockedPlayer) throws CommandSyntaxException {
        UnBlockPlayer.unBlockPlayer(source.getPlayerOrException(), blockedPlayer);
        return 1;
    }

    public static void onRegisterCommandEvent(CommandDispatcher<CommandSourceStack> dispatcher) {
        dispatcher.register(literal(Config.TPUNBLOCK_COMMAND_NAME.get())
                .then(argument("player", EntityArgument.player())
                        .executes(context -> unBlockPlayer(context.getSource(), EntityArgument.getPlayer(context, "player")))));
    }
}
