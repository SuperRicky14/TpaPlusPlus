package net.superricky.tpaplusplus.database.service

interface DataService : BlockDataService, ToggleDataService, PlayerDataService {
    fun initDataService()
    fun saveData()
}
