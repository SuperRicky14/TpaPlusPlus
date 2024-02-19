package net.superricky.tpaplusplus.commands.send;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.config.Config;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

public class TPACommand {
    private TPACommand() {
    }

    private static int teleportPlayer(CommandSourceStack source, ServerPlayer teleported) throws CommandSyntaxException {
        SendTPA.sendTeleportRequest(source.getPlayerOrException(), teleported, false);
        return 1;
    }

    public static void onRegisterCommandEvent(CommandDispatcher<CommandSourceStack> dispatcher) {
        dispatcher.register(literal(Config.TPA_COMMAND_NAME.get())
                .then(argument("player", EntityArgument.player())
                        .executes(context -> teleportPlayer(context.getSource(), EntityArgument.getPlayer(context, "player")))));
    }
}
