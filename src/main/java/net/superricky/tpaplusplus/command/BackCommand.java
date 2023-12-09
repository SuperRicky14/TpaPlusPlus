package net.superricky.tpaplusplus.command;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.superricky.tpaplusplus.Config;
import net.superricky.tpaplusplus.DeathManager;

import static net.minecraft.commands.Commands.literal;

@Mod.EventBusSubscriber
public class BackCommand {
    @SubscribeEvent
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal("back")
                .executes(context -> teleportToLastDeath(context.getSource())));
    }
    private static int teleportToLastDeath(CommandSourceStack source) throws CommandSyntaxException {
        // Return and send feedback if /back is not enabled.
        if (!(Config.BACK_COMMAND_ENABLED.get())) {
            source.sendFailure(Component.literal("§4This command has been disabled by a server administrator."));
            return 1;
        }

        ServerPlayer executor = source.getPlayerOrException();

        try {
            DeathManager.teleportToLatestDeath(executor, DeathManager.getDeathPosition(executor));
        } catch (IllegalArgumentException e) {
            executor.sendSystemMessage(Component.literal("§cCould not find your latest death position!"));
        }
        return 1;
    }
}

