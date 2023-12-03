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
import net.superricky.tpaplusplus.teleport.TeleportTo;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

@Mod.EventBusSubscriber
public class TPACommand {
    @SubscribeEvent
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
            event.getDispatcher().register(literal("tpa")
                    .then(argument("player", EntityArgument.player())
                        .executes(context -> teleportPlayer(context.getSource(), EntityArgument.getPlayer(context, "player")))));
    }

    private static int teleportPlayer(CommandSourceStack source, ServerPlayer teleported) throws CommandSyntaxException {
        TeleportManager.sendTeleportTo(new TeleportTo(source.getPlayerOrException(), teleported));
        return 1;
    }
}
