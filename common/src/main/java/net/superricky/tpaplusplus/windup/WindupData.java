package net.superricky.tpaplusplus.windup;

import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.commands.back.LevelBoundVec3;
import net.superricky.tpaplusplus.io.PlayerData;
import net.superricky.tpaplusplus.requests.Request;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

public class WindupData {
    @Nullable
    private final Request request;

    @Nullable
    private final Boolean isHereRequest;

    @Nullable
    private final LevelBoundVec3 deathPosition;

    @Nullable
    private final PlayerData playerData;

    private final AtomicInteger delay;
    private final AtomicBoolean isCancelled; // Whether the TPA request has been cancelled or not.

    private final double acceptX;
    private final double acceptY;
    private final double acceptZ;

    @NotNull
    private final CommandType type;

    @NotNull
    private final ServerPlayer[] players;

    public WindupData(@Nullable Request request, int delay, double acceptX, double acceptY, double acceptZ, @NotNull CommandType type, @NotNull ServerPlayer[] players) {
        this.request = request;
        this.acceptX = acceptX;
        this.acceptY = acceptY;
        this.acceptZ = acceptZ;
        this.isHereRequest = null;
        this.deathPosition = null;
        this.playerData = null;
        this.delay = new AtomicInteger(delay);
        this.isCancelled = new AtomicBoolean(false);
        this.type = type;
        this.players = players;
    }

    public WindupData(@Nullable PlayerData playerData, int delay, double acceptX, double acceptY, double acceptZ, @NotNull CommandType type, @NotNull ServerPlayer[] players) {
        this.acceptX = acceptX;
        this.acceptY = acceptY;
        this.acceptZ = acceptZ;
        this.request = null;
        this.isHereRequest = null;
        this.deathPosition = null;
        this.playerData = playerData;
        this.delay = new AtomicInteger(delay);
        this.isCancelled = new AtomicBoolean(false);
        this.type = type;
        this.players = players;
    }

    public WindupData(boolean isHereRequest, int delay, double acceptX, double acceptY, double acceptZ, @NotNull CommandType type, @NotNull ServerPlayer[] players) {
        this.acceptX = acceptX;
        this.acceptY = acceptY;
        this.acceptZ = acceptZ;
        this.request = null;
        this.isHereRequest = isHereRequest;
        this.deathPosition = null;
        this.playerData = null;
        this.delay = new AtomicInteger(delay);
        this.isCancelled = new AtomicBoolean(false);
        this.type = type;
        this.players = players;
    }

    public WindupData(int delay, double acceptX, double acceptY, double acceptZ, @NotNull CommandType type, @NotNull ServerPlayer[] players) {
        this.acceptX = acceptX;
        this.acceptY = acceptY;
        this.acceptZ = acceptZ;
        this.request = null;
        this.isHereRequest = null;
        this.deathPosition = null;
        this.playerData = null;
        this.delay = new AtomicInteger(delay);
        this.isCancelled = new AtomicBoolean(false);
        this.type = type;
        this.players = players;
    }

    public WindupData(@Nullable LevelBoundVec3 deathPosition, int delay, double acceptX, double acceptY, double acceptZ, @NotNull CommandType type, ServerPlayer[] players) {
        this.acceptX = acceptX;
        this.acceptY = acceptY;
        this.acceptZ = acceptZ;
        this.request = null;
        this.isHereRequest = null;
        this.deathPosition = deathPosition;
        this.playerData = null;
        this.delay = new AtomicInteger(delay);
        this.isCancelled = new AtomicBoolean(false);
        this.type = type;
        this.players = players;
    }

    @Nullable
    public Request getRequest() {
        return request;
    }

    public Boolean getHereRequest() {
        return isHereRequest;
    }

    @Nullable
    public LevelBoundVec3 getDeathPosition() {
        return deathPosition;
    }

    @Nullable
    public PlayerData getPlayerData() {
        return playerData;
    }

    public int getDelay() {
        return delay.get();
    }

    public void setDelay(int delay) {
        this.delay.set(delay);
    }

    @NotNull
    public CommandType getType() {
        return type;
    }

    public ServerPlayer[] getPlayers() {
        return players;
    }

    public AtomicBoolean getCancelled() {
        return isCancelled;
    }

    public double getAcceptX() {
        return acceptX;
    }

    public double getAcceptY() {
        return acceptY;
    }

    public double getAcceptZ() {
        return acceptZ;
    }
}
