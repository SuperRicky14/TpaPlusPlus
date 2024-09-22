package net.superricky.tpaplusplus.database

import com.google.gson.annotations.SerializedName
import java.util.*

class PlayerData(
    @SerializedName("block_players")
    val blockPlayers: MutableSet<UUID> = Collections.synchronizedSet(HashSet()),
    @SerializedName("last_death_pos")
    val lastDeathPos: DeathPos = DeathPos(),
    @SerializedName("toggle")
    var toggle: Boolean = false
)
