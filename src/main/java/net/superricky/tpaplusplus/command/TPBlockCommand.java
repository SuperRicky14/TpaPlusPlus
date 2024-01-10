package net.superricky.tpaplusplus.command;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.superricky.tpaplusplus.util.configuration.Config;
import net.superricky.tpaplusplus.util.manager.PlayerBlockingManager;
import net.superricky.tpaplusplus.util.manager.RequestManager;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

@Mod.EventBusSubscriber
public class TPBlockCommand {
    private TPBlockCommand() {
    }

    private static int blockPlayer(CommandSourceStack source, ServerPlayer blockedPlayer) throws CommandSyntaxException {
        PlayerBlockingManager.blockPlayer(source.getPlayerOrException(), blockedPlayer);
        return 1;
    }

    @SubscribeEvent
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal("tpblock")
                .then(argument("player", EntityArgument.player())
                        .executes(context -> blockPlayer(context.getSource(), EntityArgument.getPlayer(context, "player")))));
    }
}
