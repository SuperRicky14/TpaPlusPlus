package net.superricky.tpaplusplus.command

import net.superricky.tpaplusplus.GlobalConst
import net.superricky.tpaplusplus.GlobalConst.logger
import net.superricky.tpaplusplus.command.commands.*
import net.superricky.tpaplusplus.command.subcommands.RootCommand
import net.superricky.tpaplusplus.config.Config
import net.superricky.tpaplusplus.config.command.CommandEnableSpec
import net.superricky.tpaplusplus.config.command.CommandNameSpec
import net.superricky.tpaplusplus.utility.Dispatcher

object CommandRegister {
    fun registerCommands(dispatcher: Dispatcher) {
        val rootNode = RootCommand.build()

        logger.info("Register command /${GlobalConst.MOD_ID}...")
        dispatcher.root.addChild(rootNode)

        val config = Config.getConfig()

        if (config[CommandEnableSpec.backEnable]) {
            logger.info("Register command /${config[CommandNameSpec.backCommand]}...")
            dispatcher.root.addChild(BackCommand.build())
        }

        if (config[CommandEnableSpec.tpaunblockEnable]) {
            logger.info("Register command /${config[CommandNameSpec.tpaunblockCommand]}...")
            dispatcher.root.addChild(UnblockCommand.build())
        }

        if (config[CommandEnableSpec.tpablockEnable]) {
            logger.info("Register command /${config[CommandNameSpec.tpablockCommand]}...")
            dispatcher.root.addChild(BlockCommand.build())
        }

        if (config[CommandEnableSpec.tpatoggleEnable]) {
            logger.info("Register command /${config[CommandNameSpec.tpatoggleCommand]}...")
            dispatcher.root.addChild(ToggleCommand.build())
        }

        if (config[CommandEnableSpec.tpaEnable]) {
            logger.info("Register command /${config[CommandNameSpec.tpaCommand]}...")
            dispatcher.root.addChild(TpaCommand.build())
        }

        if (config[CommandEnableSpec.tpahereEnable]) {
            logger.info("Register command /${config[CommandNameSpec.tpahereCommand]}...")
            dispatcher.root.addChild(TpaHereCommand.build())
        }

        if (!config[CommandEnableSpec.tpaEnable] && !config[CommandEnableSpec.tpahereEnable]) {
            return
        }

        if (config[CommandEnableSpec.tpacceptEnable]) {
            logger.info("Register command /${config[CommandNameSpec.tpacceptCommand]}...")
            dispatcher.root.addChild(AcceptCommand.build())
        }

        if (config[CommandEnableSpec.tpadenyEnable]) {
            logger.info("Register command /${config[CommandNameSpec.tpadenyCommand]}...")
            dispatcher.root.addChild(DenyCommand.build())
        }

        if (config[CommandEnableSpec.tpacancelEnable]) {
            logger.info("Register command /${config[CommandNameSpec.tpacancelCommand]}...")
            dispatcher.root.addChild(CancelCommand.build())
        }
    }
}
