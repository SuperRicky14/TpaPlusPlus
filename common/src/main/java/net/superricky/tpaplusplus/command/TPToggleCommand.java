package net.superricky.tpaplusplus.command;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandBuildContext;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.superricky.tpaplusplus.util.configuration.Config;
import net.superricky.tpaplusplus.util.manager.saved.TPToggleManager;

import static net.minecraft.commands.Commands.literal;

public class TPToggleCommand {
    private TPToggleCommand() {
    }

    private static int teleportPlayer(CommandSourceStack source) throws CommandSyntaxException {
        TPToggleManager.toggleTP(source.getPlayerOrException());
        return 1;
    }

    public static void onRegisterCommandEvent(CommandDispatcher<CommandSourceStack> dispatcher, CommandBuildContext registry, Commands.CommandSelection selection) {
        dispatcher.register(literal(Config.TPTOGGLE_COMMAND_NAME.get())
                .executes(context -> teleportPlayer(context.getSource())));
    }
}
