package net.superricky.tpaplusplus.commands.toggle;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.superricky.tpaplusplus.config.Config;

import static net.minecraft.commands.Commands.literal;

public class TPToggleCommand {
    private TPToggleCommand() {
    }

    private static int teleportPlayer(CommandSourceStack source) throws CommandSyntaxException {
        TPToggle.toggleTPOrWait(source.getPlayerOrException());
        return 1;
    }

    public static void onRegisterCommandEvent(CommandDispatcher<CommandSourceStack> dispatcher) {
        dispatcher.register(literal(Config.TPTOGGLE_COMMAND_NAME.get())
                .executes(context -> teleportPlayer(context.getSource())));
    }
}
