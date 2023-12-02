package net.superricky.tpaplusplus.command;

import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.superricky.tpaplusplus.utils.TeleportHandler;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

@Mod.EventBusSubscriber
public class TPADenyCommand {
    @SubscribeEvent
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal("tpadeny")
                .executes(context -> {
                    return denyMostRecentTPA(context.getSource());
                })
                .then(argument("receiver", EntityArgument.player())
                        .executes(context -> {
                            return denyTPASpecified(EntityArgument.getPlayer(context, "receiver"));
                        })));
    }
    private static int denyMostRecentTPA(CommandSourceStack source) {
        TeleportHandler teleportHandler = TeleportHandler.getInstance();
        teleportHandler.denyMostRecentTPARequest(source.getPlayer());
        return 1;
    }

    private static int denyTPASpecified(ServerPlayer receiver) {
        TeleportHandler teleportHandler = TeleportHandler.getInstance();
        teleportHandler.denySpecifiedTPARequest(receiver);
        return 1;
    }
}
