package net.superricky.tpaplusplus.command

import net.superricky.tpaplusplus.command.commands.*
import net.superricky.tpaplusplus.command.subcommands.RootCommand
import net.superricky.tpaplusplus.utility.Dispatcher

object CommandRegister {
    fun registerCommands(dispatcher: Dispatcher) {
        val rootNode = RootCommand.build()

        dispatcher.root.addChild(rootNode)

        dispatcher.root.addChild(AcceptCommand.build())
        dispatcher.root.addChild(BackCommand.build())
        dispatcher.root.addChild(CancelCommand.build())
        dispatcher.root.addChild(DenyCommand.build())
        dispatcher.root.addChild(ToggleCommand.build())
        dispatcher.root.addChild(TpaCommand.build())
        dispatcher.root.addChild(TpaHereCommand.build())
        dispatcher.root.addChild(UnblockCommand.build())
        dispatcher.root.addChild(BlockCommand.build())
    }
}
