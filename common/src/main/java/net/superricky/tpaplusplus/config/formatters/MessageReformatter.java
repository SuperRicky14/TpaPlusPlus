package net.superricky.tpaplusplus.config.formatters;

import com.mojang.logging.LogUtils;
import net.superricky.tpaplusplus.TPAPlusPlus;
import org.slf4j.Logger;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class MessageReformatter {
    private static final Random RANDOM = new Random();
    private static final Logger LOGGER = LogUtils.getLogger();

    private static final List<String> LEGAL_COLORS = List.of(
            "&4", "&c", "&6", "&e", "&2", "&a", "&b", "&3",
            "&1", "&9", "&d", "&5", "&f", "&7", "&8", "&0"
    );

    private MessageReformatter() {
    }

    public static void updateColorsAndSave(List<String> configLines, String oldMainColor, String newMainColor, String oldSecondaryColor, String newSecondaryColor, String oldErrorColor, String newErrorColor) {
        List<String> updatedConfig = configLines.stream()
                .map(line -> updateLine(line, oldMainColor, newMainColor, oldSecondaryColor, newSecondaryColor, oldErrorColor, newErrorColor))
                .toList();

        writeUpdatedConfig(updatedConfig);
    }

    private static String updateLine(String line, String oldMainColor, String newMainColor, String oldSecondaryColor, String newSecondaryColor, String oldErrorColor, String newErrorColor) {
        String tempFormattedLine = line.trim().replace("\t", "");

        if (tempFormattedLine.startsWith("#")) {
            return line;
        }

        if (tempFormattedLine.startsWith("ERR_")) {
            return line.replace(oldErrorColor, newErrorColor);
        }

        return line.replace(oldMainColor, newMainColor).replace(oldSecondaryColor, newSecondaryColor);
    }

    public static List<String> loadRawConfig() {
        List<String> configLines = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader("config" + File.separator + "tpaplusplus-messages.toml"))) {
            String line;
            while ((line = br.readLine()) != null) {
                configLines.add(line);
            }
        } catch (IOException e) {
            LOGGER.error(e.toString());
        }

        return configLines;
    }

    public static String getRandomColorCode() {
        return List.copyOf(LEGAL_COLORS).get(RANDOM.nextInt(LEGAL_COLORS.size()));
    }

    public static boolean isValidColor(String color) {
        return LEGAL_COLORS.contains(color);
    }

    public static void writeUpdatedConfig(List<String> updatedConfigLines) {
        try (BufferedWriter writer = new BufferedWriter(new FileWriter("config" + File.separator + TPAPlusPlus.CONFIG_PATH))) {
            for (String line : updatedConfigLines) {
                writer.write(line);
                writer.newLine(); // Add a new line after each line
            }
        } catch (IOException e) {
            LOGGER.error(e.toString());
        }
    }
}