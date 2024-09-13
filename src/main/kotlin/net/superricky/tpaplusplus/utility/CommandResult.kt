package net.superricky.tpaplusplus.utility

enum class CommandResult(val status: Int) {
    NORMAL(1),
    SENDER_NOT_EXIST(-1),
    TARGET_NOT_EXIST(-2),
    SELF_CHECK_ERROR(-3)
}
