package net.superricky.tpaplusplus.command;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.superricky.tpaplusplus.TeleportManager;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

@Mod.EventBusSubscriber
public final class TPAAcceptCommand {
    private TPAAcceptCommand() {}

    @SubscribeEvent()
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal("tpaaccept")
                .executes(context -> acceptMostRecentTPA(context.getSource()))
                .then(argument("player", EntityArgument.player())
                        .executes(context -> acceptTPASpecified(context.getSource(), EntityArgument.getPlayer(context, "player")))));
    }
    private static int acceptMostRecentTPA(CommandSourceStack source) throws CommandSyntaxException {
        try {
            TeleportManager.getLargestTeleportRequest(source.getPlayerOrException()).accept(false);
        } catch (IllegalArgumentException e) {
            source.getPlayerOrException().sendSystemMessage(Component.literal("§cNo teleport request was found!"));
        }
        return 1;
    }

    private static int acceptTPASpecified(CommandSourceStack source, ServerPlayer teleported) throws CommandSyntaxException {
        try {
            TeleportManager.getTeleportRequestByPlayers(source.getPlayerOrException(), teleported).accept(false);
        } catch (IllegalArgumentException e) {
            source.getPlayerOrException().sendSystemMessage(Component.literal("§cNo teleport request was found!"));
        }
        return 1;
    }
}
