package net.superricky.tpaplusplus.database

data class DeathPos(
    var x: Double = 0.0,
    var y: Double = 0.0,
    var z: Double = 0.0,
    var yaw: Float = 0.0f,
    var pitch: Float = 0.0f,
    var world: String = "",
    var backed: Boolean = true
)
