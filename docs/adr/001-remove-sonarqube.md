# 001: Remove SonarQube

**Status:** Accepted\
**Date:** 23-12-2025

## Context
I don't even remember how this was configured. It was undocumented, barely used by me, and didn't end up improving the codebase.

As of today, there is zero reason for this to exist.

## Decision
Remove SonarQube

## Options Considered
**Do nothing**: Code is a liability, and there's no point having this polluting the build script for no reason. It doesn't provide practical benefit right now.\
**Document and properly integrate SonarQube**: The reason for it's integration as far as I can remember was just a naive attempt to improve code quality. There's not a reason to put it into effect.

## Consequences
**Positive:**
* One less thing to worry about
* Might speed up builds

**Negative:**
* More inconsistent code style than a proper setup of SonarQube. Why I am still removing it:
  * Code style just isn't a priority for TPA++ right now. The mod is undergoing major refactoring, and a lot of code is likely to be thrown out anyways.