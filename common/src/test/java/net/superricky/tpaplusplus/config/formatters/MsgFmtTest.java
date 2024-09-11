package net.superricky.tpaplusplus.config.formatters;

import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

import static net.superricky.tpaplusplus.util.MsgFmt.fmt;
import static org.junit.jupiter.api.Assertions.*;

public class MsgFmtTest {

    @Test
    public void testBasicReplacement() {
        Map<String, Object> values = new HashMap<>();
        values.put("name", "John");
        String result = fmt("Hello, ${name}!", values);
        assertEquals("Hello, John!", result);
    }

    @Test
    public void testMultiplePlaceholders() {
        Map<String, Object> values = new HashMap<>();
        values.put("name", "John");
        values.put("age", 30);
        String result = fmt("Name: ${name}, Age: ${age}", values);
        assertEquals("Name: John, Age: 30", result);
    }

    @Test
    public void testEmptyString() {
        String result = fmt("", new HashMap<>());
        assertEquals("", result);
    }

    @SuppressWarnings("ConstantValue")
    @Test
    public void testNullFormatString() {
        String result = fmt(null, new HashMap<>());
        assertNull(result);
    }

    @SuppressWarnings("ConstantValue")
    @Test
    public void testNullValues() {
        String result = fmt("Hello, ${name}!", null);
        assertNull(result);
    }

    @Test
    public void testMissingPlaceholderValue() {
        Map<String, Object> values = new HashMap<>();
        values.put("name", "John");
        assertThrows(IllegalArgumentException.class, () -> fmt("Hello, ${name}. Your balance is ${balance}.", values));
    }

    @Test
    public void testTooManyPlaceholders() {
        Map<String, Object> values = new HashMap<>();
        values.put("name", "John");
        values.put("lastName", "John");
        values.put("age", 46);
        values.put("favouriteColor", "Red");

        String result = fmt("Hello, ${name}! You are ${age} years old", values);

        assertEquals("Hello, John! You are 46 years old", result);
    }

    @Test
    public void testRepeatedPlaceholders() {
        Map<String, Object> values = new HashMap<>();
        values.put("name", "John");

        String result = fmt("Hello, ${name}! How are you, ${name}?", values);

        assertEquals("Hello, John! How are you, John?", result);
    }
}
