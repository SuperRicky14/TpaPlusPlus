package net.superricky.tpaplusplus.command;

import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.sun.jna.platform.win32.COM.util.annotation.ComObject;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.superricky.tpaplusplus.Config;
import net.superricky.tpaplusplus.Main;
import net.superricky.tpaplusplus.Messages;
import net.superricky.tpaplusplus.util.DeathManager;
import net.superricky.tpaplusplus.util.MessageReformatter;
import net.superricky.tpaplusplus.util.RequestManager;
import org.checkerframework.checker.units.qual.C;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Paths;
import java.util.*;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

@Mod.EventBusSubscriber
public class TPAPlusPlusCommand {

    @SubscribeEvent()
    public static void onRegisterCommandEvent(RegisterCommandsEvent event) {
        event.getDispatcher().register(literal("tpaplusplus")
                        .requires(context -> context.hasPermission(4))
                .executes(context -> version(context.getSource()))
                .then(literal("refactor")
                        .then(literal("messages")
                                .then(literal("color-set"))
                                        .then(argument("colors", StringArgumentType.string())
                                                .executes(context -> printRawMessages(context.getSource(), StringArgumentType.getString(context, "colors"))))
                                .then(literal("reset")
                                        .executes(context -> resetMessages(context.getSource(), false))
                                        .then(literal("-force")
                                                .executes(context -> resetMessages(context.getSource(), true)))))
                        .then(literal("config")
                                .then(literal("reset")
                                        .executes(context -> resetConfig(context.getSource(), false))
                                        .then(literal("-force")
                                                .executes(context -> resetConfig(context.getSource(), true))))))
                .then(literal("version")
                        .executes(context -> version(context.getSource())))
                .then(literal("reload")
                        .executes(context -> reloadEverything(context.getSource(), false))
                        .then(literal("-force")
                                .executes(context -> reloadEverything(context.getSource(), true)))
                        .then(literal("everything")
                                .executes(context -> reloadEverything(context.getSource(), false))
                                .then(literal("-force")
                                        .executes(context -> reloadEverything(context.getSource(), true))))
                        .then(literal("config")
                                .executes(context -> reloadConfig(context.getSource(), false))
                                .then(literal("-force")
                                        .executes(context -> reloadConfig(context.getSource(), true))))
                        .then(literal("messages")
                                .executes(context -> reloadMessages(context.getSource()))))
                .then(literal("license")
                        .executes(context -> printLicense(context.getSource()))));
    }

    private static int resetConfig(CommandSourceStack source, boolean force) {
        if (!force) {
            source.sendSystemMessage(Component.literal("§6Functionality for §cbacking up §6your §cconfig §6has not yet been implemented! Are you sure you wish to continue?!"));
            source.sendSystemMessage(Component.literal("§6Run this command again with the §c-force §6argument added if you wish to continue."));
            return 1;
        }
        source.sendSystemMessage(Component.literal("§6Resetting your §cconfiguration..."));
        try {
            Files.deleteIfExists(Paths.get("config" + File.separator + "tpaplusplus-common.toml"));
        } catch (NoSuchFileException e) {
            source.sendSystemMessage(Component.literal("§6File: §cconfig" + File.separator + "tpaplusplus-common.toml §6does not exist!"));
        } catch (IOException e) {
            source.sendSystemMessage(Component.literal("§cThe JVM could not access the file."));
        }
        return 1;
    }

    private static int resetMessages(CommandSourceStack source, boolean force) {
        if (!force) {
            source.sendSystemMessage(Component.literal("§6Functionality for §cbacking up §6your §cmessages §6has not yet been implemented! Are you sure you wish to continue?!"));
            source.sendSystemMessage(Component.literal("§6Run this command again with the §c-force §6argument added if you wish to continue."));
            return 1;
        }
        source.sendSystemMessage(Component.literal("§6Resetting your §cmessages..."));
        try {
            Files.deleteIfExists(Paths.get("config" + File.separator + "tpaplusplus-messages.toml"));
        } catch (NoSuchFileException e) {
            source.sendSystemMessage(Component.literal("§6File: §cconfig" + File.separator + "tpaplusplus-messages.toml §6does not exist!"));
        } catch (IOException e) {
            source.sendSystemMessage(Component.literal("§cThe JVM could not access the file."));
        }
        return 1;
    }

    private static int printRawMessages(CommandSourceStack source, String colors) {
        colors = colors.replace(" ", "");
        String[] colorList = colors.split(",");

        if (colorList.length != 6) {
            source.sendSystemMessage(Component.literal(String.format(Messages.ERR_TPAPLUSPLUS_COLORS_REQUIRE_SIX_COLORS.get(), colorList.length)));
            return 1;
        }

        for (String color : colorList) {
            if (!MessageReformatter.isValidColor(color)) {
                source.sendSystemMessage(Component.literal(String.format(Messages.ERR_TPAPLUSPLUS_COLORS_INVALID_COLORS.get(), color)));
                source.sendSystemMessage(Component.literal(String.format(Messages.ERR_TPAPLUSPLUS_COLORS_INVALID_COLORS_EXAMPLES.get(), MessageReformatter.getRandomColorCode())));
                return 1;
            }
        }

        String oldMainColor = colorList[0].replace("&", "§");
        String oldSecondaryColor = colorList[1].replace("&", "§");
        String oldErrorColor = colorList[2].replace("&", "§");
        String newMainColor = colorList[3].replace("&", "§");
        String newSecondaryColor = colorList[4].replace("&", "§");
        String newErrorColor = colorList[5].replace("&", "§");

        MessageReformatter.updateColorsAndSave(MessageReformatter.loadRawConfig(),
                oldMainColor,
                newMainColor,
                oldSecondaryColor,
                newSecondaryColor,
                oldErrorColor,
                newErrorColor);

        source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_COLORS_SUCCESS.get()));

        reloadMessages(source);

        return 1;
    }

    private static int printLicense(CommandSourceStack source) {
        source.sendSystemMessage(Component.literal("""
                §dThe MIT License (MIT)
                                
                §bCopyright (c) 2023 SuperRicky
                                
                §3Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
                                
                §bThe above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
                                
                §l§3THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
                """));
        return 1;
    }

    private static int version(CommandSourceStack source) {
        source.sendSystemMessage(Component.literal(String.format(Messages.TPAPLUSPLUS_VERSION.get(), Main.MOD_VERSION))); // send mod version to command executor
        return 1;
    }

    private static int reloadEverything(CommandSourceStack source, boolean force) {
        source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_RELOADING_EVERYTHING.get()));
        reloadMessages(source); reloadConfig(source, force);
        source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_RELOADED_EVERYTHING.get()));
        return 1;
    }

    private static int reloadMessages(CommandSourceStack source) {
        source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_RELOADING_MESSAGES.get()));
        Messages.SPEC.afterReload();
        source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_RELOADED_MESSAGES.get()));
        return 1;
    }

    private static int reloadConfig(CommandSourceStack source, boolean force) {
        if (force) {
            source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_FORCE_RELOADING_CONFIG.get()));
            Config.SPEC.afterReload();
            RequestManager.requestSet.clear();
            DeathManager.playerDeathCoordinates.clear();
            source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_FORCE_RELOADED_CONFIG.get()));
            return 1;
        }
        source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_RELOADING_CONFIG.get()));
        Config.SPEC.afterReload();
        source.sendSystemMessage(Component.literal(Messages.TPAPLUSPLUS_RELOADED_CONFIG.get()));
        return 1;
    }
}
