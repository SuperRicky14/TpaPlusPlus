package net.superricky.tpaplusplus.command;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.GameProfileArgument;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.superricky.tpaplusplus.TeleportManager;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

@Mod.EventBusSubscriber
public class TPADenyCommand {
    @SubscribeEvent
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal("tpadeny")
                .executes(context -> denyMostRecentTPA(context.getSource()))
                .then(argument("player", EntityArgument.player())
                        .executes(context -> denyTPASpecified(context.getSource() ,EntityArgument.getPlayer(context, "player")))));
    }
    private static int denyMostRecentTPA(CommandSourceStack source) throws CommandSyntaxException {
        TeleportManager.denyTeleportRequest(TeleportManager.getLargestTeleportRequest(source.getPlayerOrException()));
        return 1;
    }

    private static int denyTPASpecified(CommandSourceStack source, ServerPlayer teleported) throws CommandSyntaxException {
        TeleportManager.denyTeleportRequest(TeleportManager.getTeleportRequestByPlayers(source.getPlayerOrException(), teleported));
        return 1;
    }
}
