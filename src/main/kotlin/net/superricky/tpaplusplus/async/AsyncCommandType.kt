package net.superricky.tpaplusplus.async

import net.superricky.tpaplusplus.command.commands.*

enum class AsyncCommandType(var handler: AsyncCommand) {
    BACK(BackCommand),
    ACCEPT(AcceptCommand),
    DENY(DenyCommand),
    CANCEL(CancelCommand),
    TPA(TpaCommand),
    TPAHERE(TpaHereCommand),
    BLOCK(BlockCommand),
    TOGGLE(ToggleCommand),
    UNBLOCK(UnblockCommand)
}
