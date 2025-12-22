## How to contribute to TPA++ (for developers)
1. Just clone the repository (https://github.com/SuperRicky14/TpaPlusPlus.git) into anywhere on your computer.
2. Open up Intellij (don't know about Eclipse) and open TPA++ as a gradle project.
3. Happy Modding!

## How to open TPA++ as a Gradle Project
1. In Intellij, click File -> Open...
2. Now in the window that opened, find where you cloned TPA++
3. Then click on the file called "build.gradle". You may see more than one "build.gradle" but it is important you click on the one in the root directory. Do NOT click on the build.gradle inside common, fabric, forge, etc.
4. Just wait until gradle has finished building everything, if you open up the "Build" tab and it says BUILD SUCCESSFUL, you should be good to go.

   ### Note:
   If you get an error somewhere along the lines of "Gradle has run out of memory" try adjusting the value in the file "gradle.properties"\
   There should be a line called "org.gradle.jvmargs" near or at the very top. In here you can change the value after the -Xmx to something else.\
   It is measured in megabytes, for example 6GB would be 6144M (which it is set to by default), and 10GB would be 10240M.
   You can calculate this by doing: Megabytes of RAM = 1024 * (how many gigabytes you want to allocate to gradle).

> If Gradle fails to build, make sure your Gradle SDK is using Java 21.

## Improve build speed
**Add <org.gradle.parallel=true> and <org.gradle.usecache=true> just underneath the <org.gradle.jvmargs=-Xmx6144M> line for greatly reduced build times.** This is because Architectury uses multiple project modules, but when building the whole project, gradle supports running these builds (along with many other operations) in parallel, using all your CPU cores. The reason why it isn't already added beforehand (anymore), is because importing the gradle project completely fails when this is enabled. After you initially import the project, you can add those lines to speedup gradle.
PS: If anyone knows how to fix my build system please tell me

# Architecture Decision Records (ADRs)

We document significant architectural decisions in `/docs/adr`.
If you are proposing a complex change or a major refactor, please include an ADR in your Pull Request.
See [ADR-000](adr/000-add-architecture-decision-records.md) for context.