package net.superricky.tpaplusplus.windupcooldown.cooldown

import net.superricky.tpaplusplus.config.Config

enum class CommandType {
    BACK,
    ACCEPT,
    DENY,
    CANCEL,
    TPA,
    TPAHERE,
    BLOCK,
    TOGGLE,
    UNBLOCK;

    fun getCommandNameFromType(): String { // Should be exhaustive, don't add else {}!
        when (this) {
            BACK -> {
                return Config.BACK_COMMAND_NAME.get();
            }
            ACCEPT -> {
                return Config.TPAACCEPT_COMMAND_NAME.get();
            }
            DENY -> {
                return Config.TPADENY_COMMAND_NAME.get();
            }
            CANCEL -> {
                return Config.TPACANCEL_COMMAND_NAME.get();
            }
            TPA -> {
                return Config.TPA_COMMAND_NAME.get();
            }
            TPAHERE -> {
                return Config.TPAHERE_COMMAND_NAME.get();
            }
            BLOCK -> {
                return Config.TPBLOCK_COMMAND_NAME.get();
            }
            TOGGLE -> {
                return Config.TPTOGGLE_COMMAND_NAME.get();
            }
            UNBLOCK -> {
                return Config.TPUNBLOCK_COMMAND_NAME.get();
            }
        }
    }
}