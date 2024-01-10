### Releases are now only avaliable on Modrinth and CurseForge
* Links: [Modrinth](https://modrinth.com/mod/tpa++) | [Curseforge](https://www.curseforge.com/minecraft/mc-mods/tpaplusplus)

### About this Github
This is the place to [suggest features](https://github.com/SuperRicky14/TpaPlusPlus/issues), [contribute to the mod](https://github.com/SuperRicky14/TpaPlusPlus/pulls), or look at the "[wiki](https://github.com/SuperRicky14/TpaPlusPlus/wiki)". If you just want to download the mod, use either [Modrinth](https://modrinth.com/mod/tpa++) or [Curseforge](https://www.curseforge.com/minecraft/mc-mods/tpaplusplus)! I will be providing translation files in a later release of the mod, which will allow anyone to translate the mod into their own language, or just simply change the messages included with the mod.

### Get the latest bleeding-edge build [here](https://urls.a3d.pro/bleedingedge2)!

# What's different about TPA++?
I aim to make a fast, compatible, **customizable** TPA mod for forge. This **NOW INCLUDES** range checks:
Range Checks are a feature that allows you to customize how close the players can be to teleport (in order to prevent players from teleporting too close, say if you would like players to explore the map more, rather than just teleporting around to each other). You can specify how far away players can be (if you would like to prevent players from teleporting across the map).

The mod is by far one of the most customizable TPA mods out there. Want to change the names of the commands from /tpa, /tpahere, etc to whatever you want? You can do that in the "tpaplusplus-common.toml" config! Want to change every single message in the mod? You can also do that in the "tpaplusplus-messages.toml" config under the config folder!

What I also like about this mod is the ability to instantly and easily customize the mod to your liking, without having to dig through hundreds of lines of configuration. Many instant, useful utilities are available under the /tpaplusplus command, check out the **below documentation on the /tpaplusplus command** for help!

**The mod is fully server-side, and aims to be compatible with other mods.**

**Unlike some other mods, the mod is 100% open source!** You are welcome to help out and contribute to the mod, help document the wiki or this description page you are reading right now! **I am open to suggestions and feedback and will try to get back to you as fast as possible!**

**The mod is under an extremely permissive license ([The MIT License](https://github.com/SuperRicky14/TpaPlusPlus/blob/master/LICENSE))**

## How do I use this mod?
TPAPlusPlus currently features eight custom commands, here is a quick run-through over each of them (Command arguments marked with $ are optional!):
* `/tpa [player]` • Sends a teleport request to any player.
* `/tpahere [player]` • Sends a teleport here request to any player.
* `/tpaaccept $[player]` • Accepts the latest teleport request. If a player is specified, accepts the teleport request from that player.
* `/tpadeny $[player]` • Denies the latest teleport request. If a player is specified, denies the teleport request from that player.
* `/tpacancel $[player]` • Cancels the last teleport request that you sent. If a player is specified, cancels the teleport request to that player.
* `/tptoggle` • Toggles on/off teleport request's for the player running the command.
* `/tpblock [player]` • Prevents a player from sending teleport requests to you.
* `/tpunblock [player]` • Allows a blocked player to send teleport requests to you again.
* `/back` • After a player dies, running this command will teleport them to their latest death. This is optional and can be disabled in the config.
* `/tpaplusplus` • [This command has been moved to it's own section in the new TPAPlusPlus wiki!](https://github.com/SuperRicky14/TpaPlusPlus/wiki/TPAPlusPlus-Server-Management-Command)

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
