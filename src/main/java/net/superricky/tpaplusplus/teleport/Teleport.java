package net.superricky.tpaplusplus.teleport;

import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.Main;
import net.superricky.tpaplusplus.TeleportManager;

public interface Teleport {
    ServerPlayer executor();
    ServerPlayer teleported();
    void send();

    /**
     * @return
     * (false) if there is no existing teleport request in the hashmap
     * (true) if there is already a teleport request in the hashmap
     */
    static boolean alreadySentTeleportRequest(Teleport teleportRequest) {
        return TeleportManager.anyTeleportRequestInMap(teleportRequest);
    }

    /**
     * If a request is absolute, even if the tpa accept time in seconds is set to 0 ( disabled ), then we will still proceed with the request and instantly teleport the player over.
     * If a request is not absolute, if the tpa accept time in seconds is not disabled, then we will start a countdown to whatever it's configured to in the config,
     * once that countdown reaches 0 (code available in EventHandler.java), we will run this method with absolute turned on.
     */
    void accept(boolean absolute);
    void absoluteAccept();
    void teleport();
    void deny();
    void cancel();
}
