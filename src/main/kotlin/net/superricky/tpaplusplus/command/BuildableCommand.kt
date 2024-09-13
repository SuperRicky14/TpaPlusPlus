package net.superricky.tpaplusplus.command

import net.superricky.tpaplusplus.utility.LiteralNode

interface BuildableCommand {
    fun build(): LiteralNode
}
