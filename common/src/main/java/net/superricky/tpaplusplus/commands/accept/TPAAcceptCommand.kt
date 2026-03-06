package net.superricky.tpaplusplus.commands.accept

import com.mojang.brigadier.CommandDispatcher
import com.mojang.brigadier.context.CommandContext
import com.mojang.brigadier.exceptions.CommandSyntaxException
import net.minecraft.commands.CommandSourceStack
import net.minecraft.commands.Commands
import net.minecraft.commands.arguments.EntityArgument
import net.minecraft.server.level.ServerPlayer
import net.superricky.tpaplusplus.commands.accept.AcceptTPA.acceptTeleportRequest
import net.superricky.tpaplusplus.config.Config
import org.apache.commons.lang3.NotImplementedException

object TPAAcceptCommand {
    fun onRegisterCommandEvent(dispatcher: CommandDispatcher<CommandSourceStack>) {
        dispatcher.register(
            Commands.literal(Config.TPAACCEPT_COMMAND_NAME.get())
                .executes { context: CommandContext<CommandSourceStack> ->
                    acceptMostRecentTPA(
                        context.getSource()
                    )
                }
                .then(
                    Commands.argument("player", EntityArgument.player())
                        .executes { context: CommandContext<CommandSourceStack> ->
                            acceptTPASpecified(
                                context.getSource(),
                                EntityArgument.getPlayer(context, "player")
                            )
                        }
                )
        )
    }

    @Throws(CommandSyntaxException::class, NotImplementedException::class)
    private fun acceptMostRecentTPA(source: CommandSourceStack): Int {
        acceptTeleportRequest(source.playerOrException)
        return 1
    }

    @Throws(CommandSyntaxException::class, NotImplementedException::class)
    private fun acceptTPASpecified(source: CommandSourceStack, sender: ServerPlayer): Int {
        acceptTeleportRequest(source.playerOrException, sender)
        return 1
    }
}
