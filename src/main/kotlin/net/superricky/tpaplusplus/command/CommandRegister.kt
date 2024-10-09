package net.superricky.tpaplusplus.command

import net.superricky.tpaplusplus.GlobalConst
import net.superricky.tpaplusplus.GlobalConst.logger
import net.superricky.tpaplusplus.command.commands.*
import net.superricky.tpaplusplus.command.subcommands.RootCommand
import net.superricky.tpaplusplus.config.config.Config.get
import net.superricky.tpaplusplus.config.config.command.CommandEnableSpec
import net.superricky.tpaplusplus.utility.Dispatcher

object CommandRegister {
    fun registerCommands(dispatcher: Dispatcher) {
        val rootNode = RootCommand.build()

        logger.info("Register command /${GlobalConst.MOD_ID}...")
        dispatcher.root.addChild(rootNode)

        if (CommandEnableSpec.backEnable.get()) {
            logger.info("Register command /${BackCommand.getCommandName()}...")
            dispatcher.root.addChild(BackCommand.build())
        }

        if (CommandEnableSpec.tpaunblockEnable.get()) {
            logger.info("Register command /${UnblockCommand.getCommandName()}...")
            dispatcher.root.addChild(UnblockCommand.build())
        }

        if (CommandEnableSpec.tpablockEnable.get()) {
            logger.info("Register command /${BlockCommand.getCommandName()}...")
            dispatcher.root.addChild(BlockCommand.build())
        }

        if (CommandEnableSpec.tpatoggleEnable.get()) {
            logger.info("Register command /${ToggleCommand.getCommandName()}...")
            dispatcher.root.addChild(ToggleCommand.build())
        }

        if (CommandEnableSpec.tpaEnable.get()) {
            logger.info("Register command /${TpaCommand.getCommandName()}...")
            dispatcher.root.addChild(TpaCommand.build())
        }

        if (CommandEnableSpec.tpahereEnable.get()) {
            logger.info("Register command /${TpaHereCommand.getCommandName()}...")
            dispatcher.root.addChild(TpaHereCommand.build())
        }

        if (!CommandEnableSpec.tpaEnable.get() && !CommandEnableSpec.tpahereEnable.get()) {
            return
        }

        if (CommandEnableSpec.tpacceptEnable.get()) {
            logger.info("Register command /${AcceptCommand.getCommandName()}...")
            dispatcher.root.addChild(AcceptCommand.build())
        }

        if (CommandEnableSpec.tpadenyEnable.get()) {
            logger.info("Register command /${DenyCommand.getCommandName()}...")
            dispatcher.root.addChild(DenyCommand.build())
        }

        if (CommandEnableSpec.tpacancelEnable.get()) {
            logger.info("Register command /${CancelCommand.getCommandName()}...")
            dispatcher.root.addChild(CancelCommand.build())
        }
    }
}
