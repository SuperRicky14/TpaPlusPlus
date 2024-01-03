package net.superricky.tpaplusplus.command;

import com.mojang.brigadier.arguments.StringArgumentType;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.network.chat.Component;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.superricky.tpaplusplus.util.configuration.Config;
import net.superricky.tpaplusplus.Main;
import net.superricky.tpaplusplus.util.configuration.Messages;
import net.superricky.tpaplusplus.util.manager.DeathManager;
import net.superricky.tpaplusplus.util.configuration.formatters.MessageReformatter;
import net.superricky.tpaplusplus.util.manager.RequestManager;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

@Mod.EventBusSubscriber
public class TPAPlusPlusCommand {
    private static final String FORCE_PARAMETER = "-force";
    private static final String CONFIG_PARAMETER = "config"; // WARNING: DON'T TOUCH, THIS IS ALSO USED IN THE RESET CONFIG FUNCTIONALITY!!!!!

    @SubscribeEvent
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal("tpaplusplus")
                .executes(context -> version(context.getSource()))
                .then(literal("refactor")
                        .requires(context -> context.hasPermission(Commands.LEVEL_OWNERS))
                        .then(literal("messages")
                                .then(literal("color-set")
                                        .then(argument("colors", StringArgumentType.string())
                                                .executes(context -> refactorColorSet(context.getSource(), StringArgumentType.getString(context, "colors")))))))
                .then(literal("version")
                            .requires(context -> context.hasPermission(Commands.LEVEL_OWNERS))
                        .executes(context -> version(context.getSource())))
                .then(literal("reload")
                            .requires(context -> context.hasPermission(Commands.LEVEL_OWNERS))
                        .executes(context -> reloadConfig(context.getSource(), false))
                        .then(literal(FORCE_PARAMETER)
                                .executes(context -> reloadConfig(context.getSource(), true)))
                        .then(literal(CONFIG_PARAMETER)
                                .executes(context -> reloadConfig(context.getSource(), false))
                                .then(literal(FORCE_PARAMETER)
                                        .executes(context -> reloadConfig(context.getSource(), true)))))
                .then(literal("license")
                        .executes(context -> printLicense(context.getSource()))));
    }

    private TPAPlusPlusCommand() {
    }

    private static int printLicense(CommandSourceStack source) {
        source.sendSystemMessage(Component.literal("""
                §6The §cMIT §6License §c(MIT)

                §cCopyright §6(c) §c2023 SuperRicky

                §6Permission is §chereby granted§6, §cfree of charge§6, to §cany person obtaining a copy of this software §6and associated documentation files (the "Software"), to deal in the Software §cwithout restriction, §6including without limitation the rights to §cuse§6, §ccopy§6, §cmodify§6, §cmerge§6, §cpublish§6, §cdistribute§6, §csublicense§6, and/or §csell copies of the Software§6, and to permit persons to whom the Software is furnished to do so, §4§l§usubject to the following conditions§6:

                §4§lThe above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

                §6§lTHE SOFTWARE IS PROVIDED "AS IS", §c§lWITHOUT WARRANTY OF ANY KIND§6§l, §c§lEXPRESS OR IMPLIED§6§l, INCLUDING BUT §c§lNOT LIMITED TO §6§lTHE §c§lWARRANTIES OF MERCHANTABILITY§6§l, §c§lFITNESS §6§lFOR A §c§lPARTICULAR PURPOSE §6§lAND §c§lNONINFRINGEMENT§6§l. IN §4§lNO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
                """));
        return 1;
    }

    private static int refactorColorSet(CommandSourceStack source, String colors) {
        colors = colors.replace(" ", "");
        String[] colorList = colors.split(",");

        if (colorList.length != 6) {
            source.sendSystemMessage(Component.literal(String.format(Messages.ERR_TPAPLUSPLUS_COLORS_REQUIRE_SIX_COLORS.get(), colorList.length)));
            return 0;
        }

        for (String color : colorList) {
            if (!MessageReformatter.isValidColor(color)) {
                source.sendSystemMessage(Component.literal(String.format(Messages.ERR_TPAPLUSPLUS_COLORS_INVALID_COLORS.get(), color)));
                source.sendSystemMessage(Component.literal(String.format(Messages.ERR_TPAPLUSPLUS_COLORS_INVALID_COLORS_EXAMPLES.get(), MessageReformatter.getRandomColorCode())));
                return 0;
            }
        }

        String oldMainColor = colorList[0].replace("&", "§").toLowerCase();
        String oldSecondaryColor = colorList[1].replace("&", "§").toLowerCase();
        String oldErrorColor = colorList[2].replace("&", "§").toLowerCase();
        String newMainColor = colorList[3].replace("&", "§").toLowerCase();
        String newSecondaryColor = colorList[4].replace("&", "§").toLowerCase();
        String newErrorColor = colorList[5].replace("&", "§").toLowerCase();

        if (oldMainColor.equals(oldSecondaryColor) || newMainColor.equals(newSecondaryColor)) {
            source.sendSystemMessage(Component.literal(Messages.ERR_TPAPLUSPLUS_COLORS_CANNOT_BE_THE_SAME.get()));
            return 0;
        }

        MessageReformatter.updateColorsAndSave(MessageReformatter.loadRawConfig(),
                oldMainColor,
                newMainColor,
                oldSecondaryColor,
                newSecondaryColor,
                oldErrorColor,
                newErrorColor);

        source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_COLORS_SUCCESS.get()));

        return 1;
    }

    private static int version(CommandSourceStack source) {
        source.sendSystemMessage(Component.literal(String.format(Messages.TPAPLUSPLUS_VERSION.get(), Main.MOD_VERSION))); // send the mod's version to the command executor
        return 1;
    }

    private static int reloadConfig(CommandSourceStack source, boolean force) {
        if (force) {
            source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_FORCE_RELOADING_CONFIG.get()));
            Config.SPEC.afterReload();
            RequestManager.clearRequestSet();
            DeathManager.clearDeathCoordinates();
            source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_FORCE_RELOADED_CONFIG.get()));
            return 0;
        }
        source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_RELOADING_CONFIG.get()));
        Config.SPEC.afterReload();
        source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_RELOADED_CONFIG.get()));
        return 1;
    }
}
