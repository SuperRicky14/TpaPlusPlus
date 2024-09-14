package net.superricky.tpaplusplus.config

import com.uchuhimo.konf.ConfigSpec

object AdvancedSpec : ConfigSpec("advanced") {
    val asyncLoopRate by required<Long>()
    val unblockingTickLoop by required<Boolean>()
    val autoSaveInterval by required<Int>()
    val updateCheckInterval by required<Int>()
}
