package net.superricky.tpaplusplus.command;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandBuildContext;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.superricky.tpaplusplus.util.configuration.Config;
import net.superricky.tpaplusplus.util.manager.DeathManager;

import static net.minecraft.commands.Commands.literal;

public class BackCommand {
    public static void onRegisterCommandEvent(CommandDispatcher<CommandSourceStack> dispatcher, CommandBuildContext registry, Commands.CommandSelection selection) {
        dispatcher.register(literal(Config.BACK_COMMAND_NAME.get())
                .executes(context -> teleportToLastDeath(context.getSource())));
    }

    private static int teleportToLastDeath(CommandSourceStack source) throws CommandSyntaxException {
        DeathManager.teleportToLatestDeath(source.getPlayerOrException());
        return 1;
    }

    private BackCommand() {
    }
}

