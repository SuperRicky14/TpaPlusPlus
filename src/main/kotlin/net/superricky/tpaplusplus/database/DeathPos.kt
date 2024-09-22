package net.superricky.tpaplusplus.database

data class DeathPos(
    var x: Double = 0.0,
    var y: Double = 0.0,
    var z: Double = 0.0,
    var world: String = "",
    var backed: Boolean = true
)
