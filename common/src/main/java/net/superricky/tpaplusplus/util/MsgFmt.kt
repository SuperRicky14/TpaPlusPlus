package net.superricky.tpaplusplus.util

import java.util.function.Supplier
import java.util.regex.Matcher

private val TEMPLATE_PATTERN = """(?<!\\)\$\{(\w+)}""".toRegex().toPattern()

/**
 * An extension function that provides functionality analogous to Kotlin's string templating, but during runtime.
 * Useful for user-inputted strings.
 * @param replacements A map of placeholders (for example "name"), and their replacement (for example "John").
 * Placeholders only contain the name of the placeholder, not the ${} surrounding it.
 * Replacements should be human-readable. If replacement is a Supplier or a zero-function lambda, it will be automatically unwrapped here into a string.
 * The returned object of the Supplier/Lambda will have it's toString() method called.
 * For anything that isn't a String, Supplier, or Lambda, it's toString() method will be called.
 * @return The templated string.
 */
fun String.template(replacements: Map<String, Any>): String {
    val sb = StringBuilder()
    val matcher = TEMPLATE_PATTERN.matcher(this)

    while (matcher.find()) {
        val placeholder = matcher.group(1)
        val replacement = when (val dynamicReplacement = replacements[placeholder]) {
            is String -> dynamicReplacement
            is Supplier<*> -> dynamicReplacement.get().toString()
            is Function0<*> -> dynamicReplacement.invoke().toString()
            is Nothing? -> matcher.group()
            else -> dynamicReplacement.toString()
        }

        matcher.appendReplacement(sb, Matcher.quoteReplacement(replacement))
    }
    matcher.appendTail(sb)
    return sb.toString()
}

