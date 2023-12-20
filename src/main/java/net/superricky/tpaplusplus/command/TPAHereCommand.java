package net.superricky.tpaplusplus.command;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.superricky.tpaplusplus.teleport.TeleportHere;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

@Mod.EventBusSubscriber
public final class TPAHereCommand {
    private TPAHereCommand() {}
    @SubscribeEvent
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal("tpahere")
                .then(argument("player", EntityArgument.player())
                        .executes(context -> teleportPlayer(context.getSource(), EntityArgument.getPlayer(context, "player")))));
    }

    private static int teleportPlayer(CommandSourceStack source, ServerPlayer teleported) throws CommandSyntaxException {
        try {
            new TeleportHere(source.getPlayerOrException(), teleported).send();
        } catch (IllegalArgumentException e) {
            source.getPlayerOrException().sendSystemMessage(Component.literal("§cNo teleport request was found!"));
        }
        return 1;
    }
}