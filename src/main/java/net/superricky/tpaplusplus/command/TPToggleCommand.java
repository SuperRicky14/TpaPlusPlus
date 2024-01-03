package net.superricky.tpaplusplus.command;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.superricky.tpaplusplus.util.configuration.Config;
import net.superricky.tpaplusplus.util.manager.RequestManager;
import net.superricky.tpaplusplus.util.manager.saved.TPToggleManager;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

@Mod.EventBusSubscriber
public class TPToggleCommand {
    private TPToggleCommand() {
    }

    private static int teleportPlayer(CommandSourceStack source) throws CommandSyntaxException {
        TPToggleManager.toggleTP(source.getPlayerOrException());
        return 1;
    }

    @SubscribeEvent
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal(Config.TPTOGGLE_COMMAND_NAME.get())
                .executes(context -> teleportPlayer(context.getSource())));
    }
}
