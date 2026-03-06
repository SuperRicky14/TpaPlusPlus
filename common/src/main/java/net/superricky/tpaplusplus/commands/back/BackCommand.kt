package net.superricky.tpaplusplus.commands.back;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.superricky.tpaplusplus.config.Config;

import static net.minecraft.commands.Commands.literal;

public class BackCommand {
    public static void onRegisterCommandEvent(CommandDispatcher<CommandSourceStack> dispatcher) {
        dispatcher.register(literal(Config.BACK_COMMAND_NAME.get())
                .executes(context -> teleportToLastDeath(context.getSource())));
    }

    private static int teleportToLastDeath(CommandSourceStack source) throws CommandSyntaxException {
        Back.teleportToLatestDeath(source.getPlayerOrException());
        return 1;
    }

    private BackCommand() {
    }
}

