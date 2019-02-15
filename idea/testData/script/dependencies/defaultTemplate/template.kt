package org.jetbrains.kotlin.idea.script

import java.io.File
import kotlin.script.dependencies.*
import kotlin.script.experimental.dependencies.*
import kotlin.script.experimental.location.*
import kotlin.script.templates.ScriptTemplateDefinition

class AsyncTestLoader : AsyncDependenciesResolver {
    @Suppress("UNCHECKED_CAST")
    override fun resolveAsync(
        scriptContents: ScriptContents, environment: Environment
    ): DependenciesResolver.ResolveResult {
        //java.lang.Thread.sleep(10000)
        return ScriptDependencies.EMPTY.asSuccess()
    }
}

@ScriptExpectedLocations([ScriptExpectedLocation.Everywhere])
@ScriptTemplateDefinition(AsyncTestLoader::class, scriptFilePattern = "script.kts")
open class Template