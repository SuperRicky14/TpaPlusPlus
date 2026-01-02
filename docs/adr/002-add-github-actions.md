# 002: Add GitHub actions

**Status:** Accepted\
**Date:** 26-12-2025

## Context
TPA++ has unit tests, but they're not run on commits??
TPA++ lacks a standardized workflow for uploading to Modrinth/CurseForge
TPA++ lacks a standardized workflow for providing bleeding edge builds to the community.
Beta/Alpha builds clutter up CurseForge/Modrinth and confuse users.

## Decision
Add GitHub actions. Configure GitHub actions to run unit tests and comment the results. Configure GitHub actions to publish artifacts on commits that successfuly compile&test. 

GitHub actions enables Modrinth/CurseForge uploading, but that can come at a later date.

## Options Considered
**Do nothing**: Because these pain points have been the things really putting me off from maintaining TPA++
**Use an external system like Jenkins**: GitHub actions is just right there. The only benefit I see to an external project is no account requirement for downloading artifacts, but that can be solved with a service such as [nightly.link](https://nightly.link). For my first project with proper CI/CD, learning an external tool doesn't have proper ROI.

## Consequences
**Positive:**
* Faster iteration, no more going through the whole manual upload process for bleeding edge builds.
* Lots of room to grow (for example automatic Modrinth/CurseForge uploading)

**Negative:**
* Deeper into the GitHub ecosystem, makes it harder to pivot to Gitlab Gitea or other alternatives if ever necessary
* Requires an account to download artifacts. As seen above, I plan on mitigating this with [nightly.link](https://nightly.link).