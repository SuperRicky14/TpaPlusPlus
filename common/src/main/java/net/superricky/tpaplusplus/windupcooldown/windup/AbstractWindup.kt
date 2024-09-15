package net.superricky.tpaplusplus.windupcooldown.windup;

import net.minecraft.server.level.ServerPlayer;
import net.superricky.tpaplusplus.commands.back.LevelBoundVec3;
import net.superricky.tpaplusplus.io.PlayerData;
import net.superricky.tpaplusplus.requests.Request;
import net.superricky.tpaplusplus.windupcooldown.CommandType;
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
    private final double originalDelay;
    private final AtomicBoolean isCancelled; // Whether the TPA request has been cancelled or not.

    private final double acceptX;
    private final double acceptY;
    private final double acceptZ;

    @NotNull
    private final CommandType type;

    @NotNull
    private final ServerPlayer[] players;

    public WindupData(@Nullable Request request, double delay, double acceptX, double acceptY, double acceptZ, @NotNull CommandType type, @NotNull ServerPlayer[] players) {
        this.request = request;
        this.acceptX = acceptX;
        this.acceptY = acceptY;
        this.acceptZ = acceptZ;
        this.isHereRequest = null;
        this.deathPosition = null;
        this.playerData = null;
        this.delay = new AtomicInteger((int) delay);
        this.isCancelled = new AtomicBoolean(false);
        this.type = type;
        this.players = players;
        this.originalDelay = delay;
    }

    public WindupData(@Nullable PlayerData playerData, double delay, double acceptX, double acceptY, double acceptZ, @NotNull CommandType type, @NotNull ServerPlayer[] players) {
        this.acceptX = acceptX;
        this.acceptY = acceptY;
        this.acceptZ = acceptZ;
        this.request = null;
        this.isHereRequest = null;
        this.deathPosition = null;
        this.playerData = playerData;
        this.delay = new AtomicInteger((int) delay);
        this.isCancelled = new AtomicBoolean(false);
        this.type = type;
        this.players = players;
        this.originalDelay = delay;
    }

    public WindupData(boolean isHereRequest, double delay, double acceptX, double acceptY, double acceptZ, @NotNull CommandType type, @NotNull ServerPlayer[] players) {
        this.acceptX = acceptX;
        this.acceptY = acceptY;
        this.acceptZ = acceptZ;
        this.request = null;
        this.isHereRequest = isHereRequest;
        this.deathPosition = null;
        this.playerData = null;
        this.delay = new AtomicInteger((int) delay);
        this.isCancelled = new AtomicBoolean(false);
        this.type = type;
        this.players = players;
        this.originalDelay = delay;
    }

    public WindupData(double delay, double acceptX, double acceptY, double acceptZ, @NotNull CommandType type, @NotNull ServerPlayer[] players) {
        this.acceptX = acceptX;
        this.acceptY = acceptY;
        this.acceptZ = acceptZ;
        this.request = null;
        this.isHereRequest = null;
        this.deathPosition = null;
        this.playerData = null;
        this.delay = new AtomicInteger((int) delay);
        this.isCancelled = new AtomicBoolean(false);
        this.type = type;
        this.players = players;
        this.originalDelay = delay;
    }

    public WindupData(@Nullable LevelBoundVec3 deathPosition, double delay, double acceptX, double acceptY, double acceptZ, @NotNull CommandType type, ServerPlayer[] players) {
        this.acceptX = acceptX;
        this.acceptY = acceptY;
        this.acceptZ = acceptZ;
        this.request = null;
        this.isHereRequest = null;
        this.deathPosition = deathPosition;
        this.playerData = null;
        this.delay = new AtomicInteger((int) delay);
        this.isCancelled = new AtomicBoolean(false);
        this.type = type;
        this.players = players;
        this.originalDelay = delay;
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

    public AtomicInteger getDelay() {
        return delay;
    }

    public double getOriginalDelay() {
        return originalDelay;
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
