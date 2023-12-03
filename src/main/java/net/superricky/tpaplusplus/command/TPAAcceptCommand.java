package net.superricky.tpaplusplus.command;

import com.mojang.brigadier.exceptions.CommandExceptionType;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.GameProfileArgument;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.superricky.tpaplusplus.TeleportManager;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;

import java.util.Objects;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

@Mod.EventBusSubscriber
public class TPAAcceptCommand {
    @SubscribeEvent
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal("tpaaccept")
                        .executes(context -> acceptMostRecentTPA(context.getSource()))
                .then(argument("player", EntityArgument.player())
                        .executes(context -> acceptTPASpecified(context.getSource(), EntityArgument.getPlayer(context, "player")))));
    }
    private static int acceptMostRecentTPA(CommandSourceStack source) throws CommandSyntaxException {
        TeleportManager.acceptTeleportRequest(TeleportManager.getLargestTeleportRequest(source.getPlayerOrException()));
        return 1;
    }

    private static int acceptTPASpecified(CommandSourceStack source, ServerPlayer teleported) {
        try {
            TeleportManager.acceptTeleportRequest(TeleportManager.getTeleportRequestByPlayers(source.getPlayerOrException(), teleported));
        } catch (CommandSyntaxException e) {
            System.out.println(e);
            throw new RuntimeException(e);
        } catch (IllegalArgumentException e) {
            System.out.println(e);
            throw new RuntimeException(e);
        }
        return 1;
    }
}
