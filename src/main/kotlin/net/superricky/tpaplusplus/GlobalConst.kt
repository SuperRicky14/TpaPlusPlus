package net.superricky.tpaplusplus

import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger

object GlobalConst {
    const val LANG_FOLDER_NAME = "lang"
    const val MOD_ID = "tpaplusplus"
    const val CONFIG_FOLDER_PATH = MOD_ID
    const val LANG_FOLDER_PATH = "$CONFIG_FOLDER_PATH/$LANG_FOLDER_NAME"
    const val DEFAULT_LANG_FILE_NAME = "en_us.toml"
    const val DEFAULT_LANG_FILE_PATH = "$CONFIG_FOLDER_PATH/$LANG_FOLDER_PATH/$DEFAULT_LANG_FILE_NAME"
    const val DEFAULT_LANG_FILE_SOURCE_PATH = "$LANG_FOLDER_NAME/$DEFAULT_LANG_FILE_NAME"
    const val EXAMPLE_LANG_FILE_PATH = "$LANG_FOLDER_NAME/example.toml"
    const val CONFIG_FILE_NAME = "$MOD_ID.toml"
    const val CONFIG_FILE_PATH = "$CONFIG_FOLDER_PATH/$CONFIG_FILE_NAME"
    const val PLAYER_DATA_FILE_NAME = "$MOD_ID.json"

    const val GITHUB_URL = "https://github.com/SuperRicky14/TpaPlusPlus"
    const val MODRINTH_URL = "https://modrinth.com/mod/tpa++"
    const val COURSE_FORGE_URL = "https://www.curseforge.com/minecraft/mc-mods/tpaplusplus"

    const val NETHER_COEFFICIENT = 8
    const val SERVER_TICK_RATE = 20.0
    const val ONE_SECOND = 1000L

    const val PERMISSION_LEVEL = 3

    val logger: Logger = LogManager.getLogger(MOD_ID)
}
