package net.superricky.tpaplusplus.util

import com.mojang.logging.LogUtils
import java.util.function.Supplier
import java.util.regex.Matcher

private val TEMPLATE_PATTERN = """(\\)?\$\{(\w+)}""".toRegex().toPattern()
private val LOGGER = LogUtils.getLogger()

/**
 * The string this is called on will be templated similar to Kotlin's templating, but at runtime.
 * Placeholders are declared in the format of ${placeholder name}. They can be escaped with backslashes like so: \${placeholder name}.
 * If a replacement for a placeholder is not found, it will leave the placeholder untouched — as if it was escaped.
 * A string can contain the same placeholder multiple times, but only one replacement. However, a replacement can be a lambda or Supplier<T>.
 * Useful for user-inputted strings.
 * @param replacements A map of placeholders (for example "name"), and their replacement (for example "John").
 * Placeholders in this map only contain the name of the placeholder, not the ${} surrounding it.
 * If replacement is a Supplier or a zero-parameter lambda, it will be automatically unwrapped here into a string.
 * Suppliers or lambdas should not return null. If they do, they will be treated as if the replacement was not found.
 * For anything that isn't a String, Supplier, or Lambda, its toString() method will be called.
 * @return The templated string.
 */
fun String.template(replacements: Map<String, Any>): String {
    val sb = StringBuilder()
    val matcher = TEMPLATE_PATTERN.matcher(this)

    while (matcher.find()) {
        val group = matcher.group()

        if (matcher.group(1) != null) {
            val consumedBackslashGroup = group.substring(1)
            matcher.appendReplacement(sb, Matcher.quoteReplacement(consumedBackslashGroup))
            continue
        }

        val placeholder = matcher.group(2)
        val unsafeReplacement = replacements[placeholder]
        val replacement = getReplacementTypeSafe(group, placeholder, unsafeReplacement)

        matcher.appendReplacement(sb, Matcher.quoteReplacement(replacement))
    }
    matcher.appendTail(sb)
    return sb.toString()
}

private fun getReplacementTypeSafe(group: String, placeholder: String, unsafeReplacement: Any?): String {
    when (unsafeReplacement) {
        is String -> {
            return unsafeReplacement
        }
        is Supplier<*> -> {
            val unknownSupplierResult = unsafeReplacement.get()
            if (unknownSupplierResult == null) {
                LOGGER.warn("Attempted to unwrap Supplier \"$unsafeReplacement\" but got null. Refusing to template placeholder $placeholder")
                return group
            }
            return unknownSupplierResult.toString()
        }
        is Function0<*> -> {
            val unknownLambdaResult = unsafeReplacement.invoke()
            if (unknownLambdaResult == null) {
                LOGGER.warn("Attempted to unwrap Lambda \"$unsafeReplacement\" but got null. Refusing to template placeholder $placeholder")
                return group
            }
            return unknownLambdaResult.toString()
        }
        is Nothing? -> return group
        else -> return unsafeReplacement.toString()
    }
}