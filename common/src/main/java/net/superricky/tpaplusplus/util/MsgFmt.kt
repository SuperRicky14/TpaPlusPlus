package net.superricky.tpaplusplus.util

import java.util.regex.Matcher

private val TEMPLATE_PATTERN = """(?<!\\)\$\{(\w+)}""".toRegex().toPattern()

/**
 * An extension function that provides functionality analogous to Kotlin's string templating, but during runtime.
 * Useful for user-inputted strings.
 * @param replacements A map of placeholders (for example "name"), and their replacement (for example "John").
 * Placeholders only contain the name of the placeholder, not the ${} surrounding it.
 * Replacements should be human-readable.
 * @return The templated string.
 */
fun String.template(replacements: Map<String, String>): String {
    val sb = StringBuilder()
    val matcher = TEMPLATE_PATTERN.matcher(this)

    while (matcher.find()) {
        val placeholder = matcher.group(1)
        val replacement = replacements[placeholder] ?: matcher.group()

        matcher.appendReplacement(sb, Matcher.quoteReplacement(replacement))
    }
    matcher.appendTail(sb)
    return sb.toString()
}

