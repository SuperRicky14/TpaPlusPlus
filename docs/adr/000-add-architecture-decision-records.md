# 000: Add architecture decision records

**Status:** Accepted\
**Date:** 23-12-2025

## Context
TPA++ has become a mess of undocumented and bizarre decisions. It's impossible to reason about the "why" behind certain decisions, especially in the build script.

## Decision
Add architecture decision records for all future decisions. They will be managed manually.

## Options Considered
**Do nothing:** We could keep relying on code comments and memory. But that's led to the current mess we have now.\
**External wiki:** It tends to rot, and if the in-code documentation/comments sucks, an external wiki will surely get outdated.\
**Automated ADR's / ADR Utility:** No reason to for a small project. Adds unnecessary friction and setup cost

## Consequences
**Positive:**
* Source of truth for the "why" behind decisions.
* Easy to get started, just Markdown documents.

**Negative:**
* No automation means it's vulnerable to human error. Also means it becomes hard to manage ADR numbers with multiple contributors.
* Pretty formal for a Minecraft mod... Mitigations:
  * We will keep ADR's short, casual, and allow bullet points.
  * Formatting can be loose. Just as long as you can read it without too much effort.
  * As long as there is a log of reasoning "in the moment" during decisions, it's FINE.