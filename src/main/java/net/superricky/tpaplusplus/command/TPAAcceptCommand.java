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
import org.apache.commons.lang3.NotImplementedException;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

@Mod.EventBusSubscriber
public class TPAAcceptCommand {
    @SubscribeEvent()
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal(Config.TPAACCEPT_COMMAND_NAME.get())
                .executes(context -> acceptMostRecentTPA(context.getSource()))
                .then(argument("player", EntityArgument.player())
                        .executes(context -> acceptTPASpecified(context.getSource(), EntityArgument.getPlayer(context, "player")))));
    }

    private static int acceptMostRecentTPA(CommandSourceStack source) throws CommandSyntaxException, NotImplementedException {
        RequestManager.acceptTeleportRequest(source.getPlayerOrException());
        return 1;
    }

    private static int acceptTPASpecified(CommandSourceStack source, ServerPlayer sender) throws CommandSyntaxException, NotImplementedException {
        RequestManager.acceptTeleportRequest(source.getPlayerOrException(), sender);
        return 1;
    }

    private TPAAcceptCommand() {
    }
}
