package net.superricky.tpaplusplus.utility

import com.mojang.brigadier.CommandDispatcher
import com.mojang.brigadier.context.CommandContext
import com.mojang.brigadier.tree.LiteralCommandNode
import net.minecraft.registry.RegistryKey
import net.minecraft.server.command.ServerCommandSource
import net.minecraft.world.World

typealias Dispatcher = CommandDispatcher<ServerCommandSource>
typealias LiteralNode = LiteralCommandNode<ServerCommandSource>
typealias Context = CommandContext<ServerCommandSource>
typealias ServerDimension = RegistryKey<World>
