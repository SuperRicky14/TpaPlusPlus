package net.superricky.tpaplusplus.commands.back

import com.mojang.brigadier.CommandDispatcher
import com.mojang.brigadier.context.CommandContext
import com.mojang.brigadier.exceptions.CommandSyntaxException
import net.minecraft.commands.CommandSourceStack
import net.minecraft.commands.Commands
import net.superricky.tpaplusplus.commands.back.Back.teleportToLatestDeath
import net.superricky.tpaplusplus.config.Config

object BackCommand {
    fun onRegisterCommandEvent(dispatcher: CommandDispatcher<CommandSourceStack>) {
        dispatcher.register(
            Commands.literal(Config.BACK_COMMAND_NAME.get())
                .executes { context: CommandContext<CommandSourceStack> ->
                    teleportToLastDeath(
                        context.getSource()
                    )
                }
        )
    }

    @Throws(CommandSyntaxException::class)
    private fun teleportToLastDeath(source: CommandSourceStack): Int {
        teleportToLatestDeath(source.playerOrException)
        return 1
    }
}

