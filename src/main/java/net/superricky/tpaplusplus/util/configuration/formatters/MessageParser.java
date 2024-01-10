package net.superricky.tpaplusplus.util.configuration.formatters;

import org.jetbrains.annotations.NotNull;

import java.util.Map;

/**
 * A utility class to format messages with a basic implementation of f-strings, since String.format does not suffice!
 */
public class MessageParser {
    // black magic using substrings and index's to get something similar to f-strings in other languages.
    public static String enhancedFormatter(String formatString, Object... args) {
        // current position where we're searching for placeholders
        int startPos = 0;

        // the final formatted string we'll build
        String formattedString = formatString;

        // loop until we find no more placeholders
        while ((startPos = formattedString.indexOf("${", startPos)) != -1) {
            // find the closing brace for the current placeholder
            int endPos = formattedString.indexOf("}", startPos);

            // check if the closing brace is missing
            if (endPos == -1) {
                throw new IllegalArgumentException("Missing closing brace for placeholder: " + formattedString.substring(startPos));
            }

            //eExtract the placeholder name (without the braces)
            String placeholder = formattedString.substring(startPos + 2, endPos);

            // store the value found for the placeholder (initially null)
            Object value = null;

            // look for the matching value in the provided arguments
            for (Object arg : args) {
                // check if the value is in a Map with the corresponding key
                if (arg instanceof Map) {
                    value = ((Map<?, ?>) arg).get(placeholder);
                }

                // check if the value is directly one of the arguments with the same name
                else if (arg.getClass().getSimpleName().equalsIgnoreCase(placeholder)) {
                    value = arg;
                }
            }

            // check if a value was found for the placeholder
            if (value != null) {
                // replace the placeholder with its value in the formatted string
                formattedString = formattedString.substring(0, startPos) + value + formattedString.substring(endPos + 1);

                // adjust the starting position for the next iteration to account for the replaced text
                startPos += value.toString().length();
            } else {
                // throw an exception if the placeholder has no matching value
                throw new IllegalArgumentException("Missing value for placeholder: " + placeholder);
            }
        }

        // return the final formatted string
        return formattedString;
    }

    private MessageParser() {
    }
}
