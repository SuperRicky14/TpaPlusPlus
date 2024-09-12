package net.superricky.tpaplusplus

import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger

object Const {
    const val MOD_ID = "tpaplusplus"
    const val CONFIG_FOLDER_PATH = MOD_ID
    const val CONFIG_FILE_NAME = "$MOD_ID.toml"
    const val CONFIG_FILE_PATH = "$CONFIG_FOLDER_PATH/$CONFIG_FILE_NAME"
    val logger: Logger = LogManager.getLogger(MOD_ID)
}
