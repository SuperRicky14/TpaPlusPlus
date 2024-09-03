package net.superricky.tpaplusplus.config.formatters;

import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A utility class to format messages with a basic implementation of f-strings, since String.format doesn't have the customizablity that we want.
 */
public class MessageParser {
    private static final Pattern PLACEHOLDER_PATTERN = Pattern.compile("\\$\\{(\\w+)}");

    public static String enhancedFormatter(String formatString, Map<String, Object> values) {
        if (formatString == null || values == null) {
            return null;
        }

        StringBuilder result = new StringBuilder(formatString);
        Matcher matcher = PLACEHOLDER_PATTERN.matcher(formatString);

        while (matcher.find()) {
            String placeholder = matcher.group(1);
            Object value = values.get(placeholder);

            if (value == null) {
                throw new IllegalArgumentException("Missing value for placeholder: " + placeholder);
            }

            int start = result.indexOf("${" + placeholder + "}");
            int end = start + placeholder.length() + 3; // +3 for "${" and "}"
            result.replace(start, end, value.toString());

            // Reset matcher to account for the replaced text
            matcher.reset(result);
        }

        return result.toString();
    }

    private MessageParser() {
    }
}
