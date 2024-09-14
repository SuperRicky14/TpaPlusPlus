package net.superricky.tpaplusplus.command

import net.superricky.tpaplusplus.async.AsyncCommandData

interface AsyncCommand {
    fun checkWindupDistance(asyncCommandData: AsyncCommandData): Boolean
}
