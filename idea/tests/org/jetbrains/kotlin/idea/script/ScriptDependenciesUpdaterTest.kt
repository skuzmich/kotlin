/*
 * Copyright 2010-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.script

import org.jetbrains.kotlin.idea.KotlinFileType
import org.jetbrains.kotlin.idea.core.script.ScriptDependenciesManager
import org.jetbrains.kotlin.idea.core.script.scriptDependencies
import org.jetbrains.kotlin.idea.test.PluginTestCaseBase


class ScriptDependenciesUpdaterTest : AbstractScriptConfigurationTest() {

    // dependencies should be loaded on file open event
    fun testFileOpen() {
        assert(ScriptDependenciesManager.getInstance(myProject).getAllScriptsClasspath().isEmpty())

        configureScriptFile(testDataPath)

        assert(ScriptDependenciesManager.getInstance(myProject).getAllScriptsClasspath().isNotEmpty())
    }

    fun testFileAttributes() {
        configureByText(KotlinFileType.INSTANCE, "val a = 1", "kts")

        assert(myFile.virtualFile.scriptDependencies != null)
    }

    override fun getTestDataPath(): String {
        return PluginTestCaseBase.getTestDataPathBase() + "/script/dependencies"
    }
}
