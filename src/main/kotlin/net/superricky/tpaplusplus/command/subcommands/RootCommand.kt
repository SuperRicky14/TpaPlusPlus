package net.superricky.tpaplusplus.command.subcommands

import net.minecraft.server.command.CommandManager.literal
import net.minecraft.text.ClickEvent
import net.minecraft.text.Text
import net.superricky.tpaplusplus.GlobalConst
import net.superricky.tpaplusplus.TpaPlusPlus
import net.superricky.tpaplusplus.command.BuildableCommand
import net.superricky.tpaplusplus.utility.*

object RootCommand : BuildableCommand {
    override fun build(): LiteralNode =
        literal(GlobalConst.MOD_ID)
            .executes { showMetaData(it) }
            .build()

    @Suppress("LongMethod")
    private fun showMetaData(context: Context): Int {
        val source = context.source
        source.sendFeedback(
            {
                Text.translatable(
                    "system.version",
                    TpaPlusPlus.version.friendlyString.literal()
                        .setStyle(TextColorPallet.secondaryVariant)
                ).setStyle(TextColorPallet.primaryVariant)
            },
            false
        )
        source.sendFeedback(
            {
                Text.translatable(
                    "system.github",
                    "system.github.view".translate()
                        .setStyle(TextColorPallet.secondaryVariant)
                        .styled {
                            it.withClickEvent(
                                ClickEvent(ClickEvent.Action.OPEN_URL, GlobalConst.GITHUB_URL)
                            )
                        }
                ).setStyle(TextColorPallet.primaryVariant)
            },
            false
        )
        source.sendFeedback(
            {
                Text.translatable(
                    "system.modrinth",
                    "system.modrinth.view".translate()
                        .setStyle(TextColorPallet.secondaryVariant)
                        .styled {
                            it.withClickEvent(
                                ClickEvent(ClickEvent.Action.OPEN_URL, GlobalConst.MODRINTH_URL)
                            )
                        }
                )
                    .setStyle(TextColorPallet.primaryVariant)
            },
            false
        )
        source.sendFeedback(
            {
                Text.translatable(
                    "system.courseforge",
                    "system.courseforge.view".translate()
                        .setStyle(TextColorPallet.secondaryVariant)
                        .styled {
                            it.withClickEvent(
                                ClickEvent(ClickEvent.Action.OPEN_URL, GlobalConst.COURSE_FORGE_URL)
                            )
                        }
                )
                    .setStyle(TextColorPallet.primaryVariant)
            },
            false
        )
        return 1
    }
}
