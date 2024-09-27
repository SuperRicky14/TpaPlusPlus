import org.jetbrains.kotlin.gradle.dsl.JvmTarget
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile
import java.text.SimpleDateFormat
import java.util.*

plugins {
    alias(libs.plugins.kotlin)
    alias(libs.plugins.loom)
    alias(libs.plugins.git.hooks)
    alias(libs.plugins.detekt)
    alias(libs.plugins.shadow)
    `maven-publish`
}

val props = properties

val modId: String by project
val modName: String by project
val modVersion: String by project
val mavenGroup: String by project
val targetJavaVersion = 21
val javaVersion = JavaVersion.VERSION_21

version = "$modVersion${getVersionMetadata()}"
group = mavenGroup

repositories {
    maven("https://maven.architectury.dev/")
    maven("https://maven.nucleoid.xyz")
    mavenLocal()
    mavenCentral()
}

dependencies {
    detektPlugins(libs.detekt)
    minecraft(libs.minecraft)
    mappings(variantOf(libs.yarn.mappings) { classifier("v2") })

    modImplementation(libs.bundles.fabric)
    modImplementation(libs.architectury)
    modImplementation(libs.translations)

    // For debug
    modImplementation(libs.bundles.konf)
    modImplementation(libs.gson)

    // For release
    shadow(libs.bundles.konf)
    shadow(libs.gson)
}

base {
    archivesName.set(modName)
}

java {
    toolchain.languageVersion = JavaLanguageVersion.of(targetJavaVersion)
    sourceCompatibility = javaVersion
    targetCompatibility = javaVersion
    withSourcesJar()
}

loom {
    splitEnvironmentSourceSets()

    mods {
        register("tpaplusplus") {
            sourceSet("main")
            sourceSet("client")
        }
    }
}

tasks {
    processResources {
        val minecraftVersion: String by project
        val loaderVersion: String by project
        val kotlinLoaderVersion: String by project
        val serverTranslatesApi: String by project

        inputs.property("version", project.version)
        inputs.property("minecraft_version", minecraftVersion)
        inputs.property("loader_version", loaderVersion)
        inputs.property("server_translates_api", serverTranslatesApi)
        filteringCharset = "UTF-8"

        filesMatching("fabric.mod.json") {
            expand(
                "version" to project.version,
                "minecraft_version" to minecraftVersion,
                "loader_version" to loaderVersion,
                "kotlin_loader_version" to kotlinLoaderVersion
            )
        }
    }

    withType<JavaCompile>().configureEach {
        options.encoding = "UTF-8"
        options.release.set(targetJavaVersion)
    }

    withType<KotlinCompile>().configureEach {
        compilerOptions.jvmTarget.set(JvmTarget.fromTarget(targetJavaVersion.toString()))
    }

    jar {
        from("LICENSE")
        manifest {
            attributes(
                "Build-By" to System.getProperty("user.name"),
                "Build-TimeStamp" to SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ").format(Date()),
                "Build-Version" to version,
                "Created-By" to "Gradle ${gradle.gradleVersion}",
                "Build-Jdk" to "${System.getProperty("java.version")} " +
                        "(${System.getProperty("java.vendor")} ${System.getProperty("java.vm.version")})",
                "Build-OS" to "${System.getProperty("os.name")} " +
                        "${System.getProperty("os.arch")} ${System.getProperty("os.version")}"
            )
        }
    }

    shadowJar {
        from("LICENSE")

        configurations = listOf(
            project.configurations.shadow.get()
        )
        archiveClassifier.set("dev-all")

        exclude("kotlin/**", "kotlinx/**", "javax/**")
        exclude("org/intellij/**", "org/jetbrains/annotations/**")
        exclude("org/slf4j/**")
        exclude("net/kyori/**")
        exclude("org/slf4j/**")

        val relocatePath = "net.superricky.tpaplusplus.libs."
        relocate("com.moandjiezana.toml", relocatePath + "com.moandjiezana.toml")
    }

    remapJar {
        dependsOn(shadowJar)
        inputFile = shadowJar.get().archiveFile
    }
}

publishing {
    publications {
        create<MavenPublication>("mavenJava") {
            artifactId = modName
            from(components["java"])
        }
    }

    repositories {
    }
}

detekt {
    buildUponDefaultConfig = true
    autoCorrect = true
    config.setFrom(rootProject.files("detekt.yml"))
}

gitHooks {
    setHooks(
        mapOf("pre-commit" to "detekt")
    )
}

fun getVersionMetadata(): String {
    val buildId = System.getenv("GITHUB_RUN_NUMBER")
    val workflow = System.getenv("GITHUB_WORKFLOW")

    if (workflow == "Release") {
        return ""
    }

    // CI builds only
    if (buildId != null) {
        return "+build.$buildId"
    }

    // No tracking information could be found about the build
    return "+nightly"
}
