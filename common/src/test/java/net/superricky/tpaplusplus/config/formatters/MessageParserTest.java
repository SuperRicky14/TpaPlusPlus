package net.superricky.tpaplusplus.config.formatters;

import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

import static net.superricky.tpaplusplus.config.formatters.MessageParser.enhancedFormatter;
import static org.junit.jupiter.api.Assertions.*;

public class MessageParserTest {

    @Test
    public void testBasicReplacement() {
        Map<String, Object> values = new HashMap<>();
        values.put("name", "John");
        String result = enhancedFormatter("Hello, ${name}!", values);
        assertEquals("Hello, John!", result);
    }

    @Test
    public void testMultiplePlaceholders() {
        Map<String, Object> values = new HashMap<>();
        values.put("name", "John");
        values.put("age", 30);
        String result = enhancedFormatter("Name: ${name}, Age: ${age}", values);
        assertEquals("Name: John, Age: 30", result);
    }

    @Test
    public void testEmptyString() {
        String result = enhancedFormatter("", new HashMap<>());
        assertEquals("", result);
    }

    @SuppressWarnings("ConstantValue")
    @Test
    public void testNullFormatString() {
        String result = enhancedFormatter(null, new HashMap<>());
        assertNull(result);
    }

    @SuppressWarnings("ConstantValue")
    @Test
    public void testNullValues() {
        String result = enhancedFormatter("Hello, ${name}!", null);
        assertNull(result);
    }

    @Test
    public void testMissingPlaceholderValue() {
        Map<String, Object> values = new HashMap<>();
        values.put("name", "John");
        assertThrows(IllegalArgumentException.class, () -> enhancedFormatter("Hello, ${name}. Your balance is ${balance}.", values));
    }

    @Test
    public void testTooManyPlaceholders() {
        Map<String, Object> values = new HashMap<>();
        values.put("name", "John");
        values.put("lastName", "John");
        values.put("age", 46);
        values.put("favouriteColor", "Red");

        String result = enhancedFormatter("Hello, ${name}! You are ${age} years old", values);

        assertEquals("Hello, John! You are 46 years old", result);
    }
}
