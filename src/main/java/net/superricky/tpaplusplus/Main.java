package net.superricky.tpaplusplus;

import com.mojang.authlib.GameProfile;
import net.minecraft.client.Minecraft;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.superricky.tpaplusplus.event.TeleportRequestTimeoutEvent;
import net.superricky.tpaplusplus.teleport.Teleport;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

// The value here should match an entry in the META-INF/mods.toml file
@Mod(Main.MOD_ID)
public class Main {
    public static final Integer teleportRequestTimeoutTime = 60 * 20;
    public static final String MOD_ID = "tpaplusplus";

    // The teleport request is our Teleport part, the time remaining (in ticks) is the Integer part.
    public static HashMap<Teleport, Integer> teleportRequests = new HashMap<>();

    public Main() {
        IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        MinecraftForge.EVENT_BUS.register(this);

        // Register Custom Events
        //MinecraftForge.EVENT_BUS.register(EventHandler.class);
    }
}
