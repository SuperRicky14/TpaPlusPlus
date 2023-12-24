package _TESTING.superricky.tpaplusplus.test.command;

import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import _TESTING.superricky.tpaplusplus.test._Config;
import _TESTING.superricky.tpaplusplus.test._Main;

import static net.minecraft.commands.Commands.literal;

@Mod.EventBusSubscriber
public class _TPAPlusPlusCommand {
    @SubscribeEvent()
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal("tpaplusplus")
                        .requires((context) -> context.hasPermission(2))
                .executes(context -> version(context.getSource()))
                .then(literal("version")
                        .executes(context -> version(context.getSource())))
                .then(literal("reload")
                        .executes(context -> reloadConfig(context.getSource()))));
    }
    private static int version(CommandSourceStack source) {
        source.sendSystemMessage(Component.literal("§6You are running TPAPlusPlus version §c" + _Main.MOD_VERSION)); // send mod version to command executor
        return 1;
    }

    private static int reloadConfig(CommandSourceStack source) {
        source.sendSystemMessage(Component.literal("§6Reloading §cConfig§6..."));
        _Config.SPEC.afterReload();
        source.sendSystemMessage(Component.literal("§6Done!"));
        return 1;
    }
}
