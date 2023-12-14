### Links: [Modrinth](https://modrinth.com/mod/tpa++) | [Curseforge](https://www.curseforge.com/minecraft/mc-mods/tpaplusplus)

### About this Github
This is the place to [suggest features](https://github.com/SuperRicky14/TpaPlusPlus/issues), [contribute to the mod](https://github.com/SuperRicky14/TpaPlusPlus/pulls), or look at the "[wiki](https://github.com/SuperRicky14/TpaPlusPlus/wiki)". If you just want to download the mod, use either [Modrinth](https://modrinth.com/mod/tpa++) or [Curseforge](https://www.curseforge.com/minecraft/mc-mods/tpaplusplus)! I will be providing translation files in a later release of the mod, which will allow anyone to translate the mod into their own language, or just simply change the messages included with the mod.

### Get the latest bleeding-edge build [here](https://github.com/SuperRicky14/TpaPlusPlus/releases/tag/v1.3-1.20.x-BLEEDING_EDGE)!

# What's different about TPA++?
I aim to make a fast, compatible, customizable TPA mod for forge. This will eventually include range checks:
Range Checks are a feature that allows you to customize how close the players can be to teleport ( in order to prevent players from teleporting too close, say if you would like players to explore the map more, rather than just teleporting around to each other ). You can specify how far away players can be (if you would like to prevent players from teleporting across the map).
Another balancing feature we would like to include is item / exp removal: This balance feature will be in place to remove any amount of item(s) from vanilla or modded mc, and / or any amount of experience from the player.
The mod aims to be fully serverside, and compatible with other mods.

## How do I use this mod?
* /tpa | This command will send a teleport request to the other player!
* /tpahere | This command will send a request to teleport the other player to you!
* /tpaaccept | This command is used to accept a teleport request!
* /tpadeny | This command is used to deny someones request to teleport to you!
* /tpacancel | Used by the initiator of the teleport request, this command will cancel the teleport request before it can be accepted by the other player!
* /back | When executed by a player after their death, it will teleport them directly back to the location of their latest death. Once they use this command, they will no longer be able to teleport to that same death! (This can be disabled in the config)
* /tpaplusplus (*new!*) | When executed by an operator, it will display the current version of the mod that was installed. If the operator runs "/tpaplusplus reload", then it will reload the configuration during runtime! All options do not require a restart, and you can change / modify them whenever you want, once you reload the config, all those settings will be updated! ( changing the timeout time and the time until a tpa request accept will only work for teleport requests sent AFTER the configuration was changed. )
## Why did I make this?
I made TPA++ due to the lack of TPA mods for forge, since I wanted to add TPA to my smp I was making.

## Frequently Asked Questions:
* Will you port the mod to Fabric?
        Currently no, but I plan to in the future!
* Can you port to version X?
        If it is a new version, it is coming unless I explicitly say otherwise. If it's an old version, no.
* Can you add feature X?
        You are welcome to request features / enhancements, I am always open to suggestions on the mods github!
* Does this mod work in multiplayer?
        Yes, but it is only required on the server. Players on vanilla mc or without the mod installed will still have 100% functionality with the mod. You only need it on the client if you wish to use it in singleplayer / on LAN network.
* I found a bug / problem with the mod.
        You can report this to me on my github. Please make sure you follow the issue guidelines.
* Can I modify / fork your mod?
        Yes, you are welcome to. However, please make sure to credit me and link this original projects [Modrinth](https://modrinth.com/mod/tpa++), [Curseforge](https://curseforge.com/minecraft/mc-mods/tpaplusplus), and / or [Github Repo](https://github.com/SuperRicky14/TpaPlusPlus) somewhere in your project. This project is protected by the [MIT license](https://github.com/SuperRicky14/TpaPlusPlus/blob/master/LICENSE).
