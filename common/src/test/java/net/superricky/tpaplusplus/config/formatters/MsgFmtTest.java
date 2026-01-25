package net.superricky.tpaplusplus.config.formatters;

import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

import static net.superricky.tpaplusplus.util.MsgFmtKt.template;
import static org.junit.jupiter.api.Assertions.*;

public class MsgFmtTest {

    @Test
    public void testBasicReplacement() {
        Map<String, Object> values = new HashMap<>();
        values.put("name", "John");
        String result = template("Hello, ${name}!", values);
        assertEquals("Hello, John!", result);
    }

    @Test
    public void testEscapingPlaceholders() {
        Map<String, Object> values = new HashMap<>();
        values.put("name", "John");
        String result = template("Hello, \\${name}!", values);
        assertEquals("Hello, ${name}!", result);
    }

    @Test
    public void testEscapingPlaceholderBackslashRemoval() {
        Map<String, Object> values = new HashMap<>();
        values.put("name", "John");
        String result = template("Hello, \\\\${name}!", values);
        assertEquals("Hello, \\${name}!", result);
    }

    // TODO: Test Suppliers and Lambdas

    @Test
    public void testMultiplePlaceholders() {
        Map<String, Object> values = new HashMap<>();
        values.put("name", "John");
        values.put("age", 30);
        String result = template("Name: ${name}, Age: ${age}", values);
        assertEquals("Name: John, Age: 30", result);
    }

    @Test
    public void testNoPlaceholders() {
        Map<String, Object> values = new HashMap<>();
        values.put("name", "John");

        String result = template("Hello there!", values);

        assertEquals("Hello there!", result);
    }

    @Test
    public void testEmptyString() {
        String result = template("", new HashMap<>());
        assertEquals("", result);
    }

    @Test
    public void testMissingPlaceholderReplacement() {
        Map<String, Object> values = new HashMap<>();
        values.put("name", "John");

        String result = template("Hello, ${name}. Your balance is ${balance}.", values);

        assertEquals("Hello, John. Your balance is ${balance}.", result);
    }

    @Test
    public void testTooManyPlaceholders() {
        Map<String, Object> values = new HashMap<>();
        values.put("name", "John");
        values.put("lastName", "John");
        values.put("age", 46);
        values.put("favouriteColor", "Red");

        String result = template("Hello, ${name}! You are ${age} years old", values);

        assertEquals("Hello, John! You are 46 years old", result);
    }

    @Test
    public void testRepeatedPlaceholders() {
        Map<String, Object> values = new HashMap<>();
        values.put("name", "John");

        String result = template("Hello, ${name}! How are you, ${name}?", values);

        assertEquals("Hello, John! How are you, John?", result);
    }
}
