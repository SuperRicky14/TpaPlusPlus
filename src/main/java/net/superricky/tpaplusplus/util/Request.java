package net.superricky.tpaplusplus.util;

import net.minecraft.server.level.ServerPlayer;

public class Request {
    private final ServerPlayer sender;
    private final ServerPlayer receiver;
    private final boolean hereRequest;
    private boolean accepted;
    private double acceptX, acceptY, acceptZ;

    public Request(ServerPlayer sender, ServerPlayer receiver, boolean hereRequest) {
        this.sender = sender;
        this.receiver = receiver;
        this.hereRequest = hereRequest;
        this.accepted = false;
    }

    public ServerPlayer getSender() {
        return sender;
    }

    public ServerPlayer getReceiver() {
        return receiver;
    }

    public boolean isHereRequest() {
        return hereRequest;
    }

    public boolean isAccepted() {
        return accepted;
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

    public void setAccepted(boolean accepted) {
        this.accepted = accepted;
    }

    public void setAcceptPosition(double acceptX, double acceptY, double acceptZ) {
        this.acceptX = acceptX;
        this.acceptY = acceptY;
        this.acceptZ = acceptZ;
    }

    @Override
    public String toString() {
        return "Sender:" + sender.getName() + ", Receiver:" + receiver.getName() + ", isHereRequest:" + hereRequest + ", isAccepted:" + accepted;
    }
}
