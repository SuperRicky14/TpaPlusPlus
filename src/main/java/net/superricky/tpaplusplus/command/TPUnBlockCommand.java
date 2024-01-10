package net.superricky.tpaplusplus.command;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.superricky.tpaplusplus.util.manager.PlayerBlockingManager;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

@Mod.EventBusSubscriber
public class TPUnBlockCommand {
    private TPUnBlockCommand() {
    }

    private static int unBlockPlayer(CommandSourceStack source, ServerPlayer blockedPlayer) throws CommandSyntaxException {
        PlayerBlockingManager.unBlockPlayer(source.getPlayerOrException(), blockedPlayer);
        return 1;
    }

    @SubscribeEvent
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal("tpunblock")
                .then(argument("player", EntityArgument.player())
                        .executes(context -> unBlockPlayer(context.getSource(), EntityArgument.getPlayer(context, "player")))));
    }
}
