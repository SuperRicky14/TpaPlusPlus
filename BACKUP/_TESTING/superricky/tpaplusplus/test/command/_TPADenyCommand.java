package _TESTING.superricky.tpaplusplus.test.command;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import _TESTING.superricky.tpaplusplus.test.teleport.manager._TeleportHelper;
import _TESTING.superricky.tpaplusplus.test.teleport.manager._TeleportManager;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

@Mod.EventBusSubscriber
public class _TPADenyCommand {
    @SubscribeEvent
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal("tpadeny")
                .executes(context -> denyMostRecentTPA(context.getSource()))
                .then(argument("player", EntityArgument.player())
                        .executes(context -> denyTPASpecified(context.getSource() ,EntityArgument.getPlayer(context, "player")))));
    }
    private static int denyMostRecentTPA(CommandSourceStack source) throws CommandSyntaxException {
        try {
            _TeleportManager.denyTeleportRequest(_TeleportHelper.getTeleportRequestByTeleported(source.getPlayerOrException()));
        } catch (IllegalArgumentException e) {
            source.getPlayerOrException().sendSystemMessage(Component.literal("§cNo teleport request was found!"));
        }
        return 1;
    }

    private static int denyTPASpecified(CommandSourceStack source, ServerPlayer teleported) throws CommandSyntaxException {
        try {
            _TeleportManager.denyTeleportRequest(_TeleportHelper.getTeleportRequestByTeleportedExecutor(source.getPlayerOrException(), teleported));
        } catch (IllegalArgumentException e) {
            source.getPlayerOrException().sendSystemMessage(Component.literal("§cNo teleport request was found!"));
        } catch (Exception e) {
            source.getPlayerOrException().sendSystemMessage(Component.literal("An unknown error occurred when searching for TPA request"));
        }
        return 1;
    }
}
