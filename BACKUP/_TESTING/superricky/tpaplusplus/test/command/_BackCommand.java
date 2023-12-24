package _TESTING.superricky.tpaplusplus.test.command;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import _TESTING.superricky.tpaplusplus.test._Config;
import _TESTING.superricky.tpaplusplus.test._DeathManager;

import static net.minecraft.commands.Commands.literal;

@Mod.EventBusSubscriber
public class _BackCommand {
    @SubscribeEvent
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal("back")
                .executes(context -> teleportToLastDeath(context.getSource())));
    }
    private static int teleportToLastDeath(CommandSourceStack source) throws CommandSyntaxException {
        // Return and send feedback if /back is not enabled.
        if (!(_Config.BACK_COMMAND_ENABLED.get())) {
            source.sendFailure(Component.literal("§cThis command has been disabled by a server administrator."));
            return 1;
        }

        ServerPlayer executor = source.getPlayerOrException();

        try {
            _DeathManager.teleportToLatestDeath(executor, _DeathManager.getDeathPosition(executor));
        } catch (IllegalArgumentException e) {
            executor.sendSystemMessage(Component.literal("§cCould not find your latest death position!"));
        }
        return 1;
    }
}

