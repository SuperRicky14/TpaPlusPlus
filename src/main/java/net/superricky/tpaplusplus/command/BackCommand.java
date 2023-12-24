package net.superricky.tpaplusplus.command;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.superricky.tpaplusplus.Config;
import net.superricky.tpaplusplus.Messages;
import net.superricky.tpaplusplus.util.DeathManager;

import static net.minecraft.commands.Commands.literal;

@Mod.EventBusSubscriber
public class BackCommand {
    @SubscribeEvent
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal("back")
                .executes(context -> teleportToLastDeath(context.getSource())));
    }

    private static int teleportToLastDeath(CommandSourceStack source) throws CommandSyntaxException {
        DeathManager.teleportToLatestDeath(source.getPlayerOrException());
        return 1;
    }
}

