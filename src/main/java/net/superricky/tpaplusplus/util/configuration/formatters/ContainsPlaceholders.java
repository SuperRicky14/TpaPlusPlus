package net.superricky.tpaplusplus.util.configuration.formatters;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.SOURCE)
@interface ContainsPlaceholders {
    String[] value(); // Array of placeholders
}
