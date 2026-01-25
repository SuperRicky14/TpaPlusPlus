package net.superricky.tpaplusplus.util;

import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A utility class to format messages with a basic implementation of f-strings, since String.format() doesn't have the flexibility that we want.
 * The acronym in the class name stands for MessageFormat. An acronym is used because it doesn't reduce readability, and this method is used extensively throughout the codebase.
 */
public class MsgFmt {
    private static final Pattern PLACEHOLDER_PATTERN = Pattern.compile("\\$\\{(\\w+)}");

    /**
     * A function acting similar to "F-Strings" in other languages like Python or C#.
     * This function takes a string and a map of placeholders to values, and replaces all instances of the placeholders with the values in their respective key-value store.
     * The "fmt" in the function name is an acronym for "Format".
     * @param formatString The string which will be formatted, containing the placeholders names. For example: "Hello, ${name}!".
     * @param values A map of keywords (for example "John"), and values implementing toString(). Typically provided with Map.of() in Java, although any map will suffice. For example, a map of ("name": "John").
     * @return The formatted string after applying all placeholders (values) to the formatString.
     */
    public static String fmt(String formatString, Map<String, Object> values) {
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

    private MsgFmt() {
    }
}
