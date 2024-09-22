package net.superricky.tpaplusplus.database.service

import java.util.*

interface ToggleDataService {
    fun checkPlayerToggle(uuid: UUID): Boolean
    fun playerSwitchToggle(uuid: UUID): Boolean
    fun playerSwitchToggle(uuid: UUID, toggle: Boolean): Boolean
}
