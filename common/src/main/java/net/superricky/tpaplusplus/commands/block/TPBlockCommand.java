package net.superricky.tpaplusplus.commands.block;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Config;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

public class TPBlockCommand {
    private TPBlockCommand() {
    }

    private static int blockPlayer(CommandSourceStack source, ServerPlayer blockedPlayer) throws CommandSyntaxException {
        BlockPlayer.blockPlayer(source.getPlayerOrException(), blockedPlayer);
        return 1;
    }

    public static void onRegisterCommandEvent(CommandDispatcher<CommandSourceStack> dispatcher) {
        dispatcher.register(literal(Config.TPBLOCK_COMMAND_NAME.get())
                .then(argument("player", EntityArgument.player())
                        .executes(context -> blockPlayer(context.getSource(), EntityArgument.getPlayer(context, "player")))));
    }
}
