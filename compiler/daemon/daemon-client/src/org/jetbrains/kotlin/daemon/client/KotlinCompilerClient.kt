/*
 * Copyright 2010-2015 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.jetbrains.kotlin.daemon.client

import kotlinx.coroutines.runBlocking
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.daemon.client.impls.KotlinCompilerClientImpl.DAEMON_DEFAULT_STARTUP_TIMEOUT_MS
import org.jetbrains.kotlin.daemon.client.impls.isProcessAlive
import org.jetbrains.kotlin.daemon.client.impls.report
import org.jetbrains.kotlin.daemon.common.*
import org.jetbrains.kotlin.daemon.common.Profiler
import org.jetbrains.kotlin.daemon.common.impls.*
import java.io.File
import java.io.PrintStream
import java.io.Serializable
import java.util.concurrent.Semaphore
import java.util.concurrent.TimeUnit
import kotlin.concurrent.thread

data class CompileServiceSession(val compileService: CompileServiceAsync, val sessionId: Int)

fun org.jetbrains.kotlin.daemon.client.impls.CompileServiceSession.toWrapper() =
    CompileServiceSession(this.compileService.toClient(), this.sessionId)

class KotlinCompilerClient : KotlinCompilerDaemonClient {

    private val oldKotlinCompilerClient = org.jetbrains.kotlin.daemon.client.impls.KotlinCompilerClientImpl

    override fun getOrCreateClientFlagFile(daemonOptions: DaemonOptions) = oldKotlinCompilerClient.getOrCreateClientFlagFile(daemonOptions)

    override suspend fun connectToCompileService(
        compilerId: CompilerId,
        daemonJVMOptions: DaemonJVMOptions,
        daemonOptions: DaemonOptions,
        reportingTargets: DaemonReportingTargets,
        autostart: Boolean,
        checkId: Boolean
    ): CompileServiceAsync? = oldKotlinCompilerClient.connectToCompileService(
        compilerId,
        daemonJVMOptions,
        daemonOptions,
        reportingTargets,
        autostart,
        checkId
    )?.toClient()

    override suspend fun connectToCompileService(
        compilerId: CompilerId,
        clientAliveFlagFile: File,
        daemonJVMOptions: DaemonJVMOptions,
        daemonOptions: DaemonOptions,
        reportingTargets: DaemonReportingTargets,
        autostart: Boolean
    ): CompileServiceAsync? = oldKotlinCompilerClient.connectToCompileService(
        compilerId,
        clientAliveFlagFile,
        daemonJVMOptions,
        daemonOptions,
        reportingTargets,
        autostart
    )?.toClient()

    override suspend fun connectAndLease(
        compilerId: CompilerId,
        clientAliveFlagFile: File,
        daemonJVMOptions: DaemonJVMOptions,
        daemonOptions: DaemonOptions,
        reportingTargets: DaemonReportingTargets,
        autostart: Boolean,
        leaseSession: Boolean,
        sessionAliveFlagFile: File?
    ): CompileServiceSession? = oldKotlinCompilerClient.connectAndLease(
        compilerId,
        clientAliveFlagFile,
        daemonJVMOptions,
        daemonOptions,
        reportingTargets,
        autostart,
        leaseSession,
        sessionAliveFlagFile
    )?.toWrapper()

    override suspend fun shutdownCompileService(compilerId: CompilerId, daemonOptions: DaemonOptions) =
        oldKotlinCompilerClient.shutdownCompileService(compilerId, daemonOptions)

    override suspend fun leaseCompileSession(compilerService: CompileServiceAsync, aliveFlagPath: String?): Int =
        oldKotlinCompilerClient.leaseCompileSession(compilerService.toRMI(), aliveFlagPath)

    override suspend fun releaseCompileSession(
        compilerService: CompileServiceAsync,
        sessionId: Int
    ) = runBlocking {
        oldKotlinCompilerClient.releaseCompileSession(compilerService.toRMI(), sessionId)
        CompileService.CallResult.Ok() // TODO
    }

    fun Profiler.toRMI() = object : org.jetbrains.kotlin.daemon.common.impls.Profiler {

        override fun getCounters() = this@toRMI.getCounters()

        override fun getTotalCounters() = this@toRMI.getTotalCounters()

        override fun <R> withMeasure(obj: Any?, body: () -> R): R = runBlocking {
            this@toRMI.withMeasure(obj) {
                body()
            }
        }

    }

    override suspend fun compile(
        compilerService: CompileServiceAsync,
        sessionId: Int,
        targetPlatform: CompileService.TargetPlatform,
        args: Array<out String>,
        messageCollector: MessageCollector,
        outputsCollector: ((File, List<File>) -> Unit)?,
        compilerMode: CompilerMode,
        reportSeverity: ReportSeverity,
        profiler: Profiler
    ) = runBlocking {
        oldKotlinCompilerClient.compile(
            compilerService.toRMI(),
            sessionId,
            targetPlatform,
            args,
            messageCollector,
            outputsCollector,
            compilerMode,
            reportSeverity,
            SOCKET_ANY_FREE_PORT,
            profiler.toRMI()
        )
    }

    interface CompilationResultsServSideCompatible : CompilationResults {

    }

    private fun CompilationResultsServSideCompatible.toServer() =
        object : CompilationResultsAsync {
            override val clientSide: CompilationResultsAsync
                get() = this

            override suspend fun add(compilationResultCategory: Int, value: Serializable) =
                this@toServer.add(compilationResultCategory, value)
        }

    override fun createCompResults(): CompilationResultsAsync {
        val oldCompResults = object : CompilationResultsServSideCompatible {

            private val resultsMap = hashMapOf<Int, MutableList<Serializable>>()

            override fun add(compilationResultCategory: Int, value: Serializable) {
                synchronized(this) {
                    resultsMap.putIfAbsent(compilationResultCategory, mutableListOf())
                    resultsMap[compilationResultCategory]!!.add(value)
                    // TODO logger?
                }
            }

        }
        return oldCompResults.toServer()
    }


    private fun startDaemon(compilerId: CompilerId, daemonJVMOptions: DaemonJVMOptions, daemonOptions: DaemonOptions, reportingTargets: DaemonReportingTargets): Boolean {
        val javaExecutable = File(File(System.getProperty("java.home"), "bin"), "java")
        val serverHostname = System.getProperty(JAVA_RMI_SERVER_HOSTNAME) ?: error("$JAVA_RMI_SERVER_HOSTNAME is not set!")
        val platformSpecificOptions = listOf(
                // hide daemon window
                "-Djava.awt.headless=true",
                "-D$JAVA_RMI_SERVER_HOSTNAME=$serverHostname")
        val args = listOf(
                javaExecutable.absolutePath, "-cp", compilerId.compilerClasspath.joinToString(File.pathSeparator)) +
                platformSpecificOptions +
                daemonJVMOptions.mappers.flatMap { it.toArgs("-") } +
                COMPILER_DAEMON_CLASS_FQN +
                daemonOptions.mappers.flatMap { it.toArgs(COMPILE_DAEMON_CMDLINE_OPTIONS_PREFIX) } +
                compilerId.mappers.flatMap { it.toArgs(COMPILE_DAEMON_CMDLINE_OPTIONS_PREFIX) }
        reportingTargets.report(DaemonReportCategory.DEBUG, "starting the daemon as: " + args.joinToString(" "))
        val processBuilder = ProcessBuilder(args)
        processBuilder.redirectErrorStream(true)
        val workingDir = File(daemonOptions.runFilesPath).apply { mkdirs() }
        processBuilder.directory(workingDir)
        // assuming daemon process is deaf and (mostly) silent, so do not handle streams
        val daemon = launchProcessWithFallback(processBuilder, reportingTargets, "daemon client")


        val isEchoRead = Semaphore(1)
        isEchoRead.acquire()

        val stdoutThread =
                thread {
                    try {
                        daemon.inputStream
                                .reader()
                                .forEachLine {
                                    if (it == COMPILE_DAEMON_IS_READY_MESSAGE) {
                                        reportingTargets.report(DaemonReportCategory.DEBUG, "Received the message signalling that the daemon is ready")
                                        isEchoRead.release()
                                        return@forEachLine
                                    }
                                    else {
                                        reportingTargets.report(DaemonReportCategory.INFO, it, "daemon")
                                    }
                                }
                    }
                    finally {
                        daemon.inputStream.close()
                        daemon.outputStream.close()
                        daemon.errorStream.close()
                        isEchoRead.release()
                    }
                }
        try {
            // trying to wait for process
            val daemonStartupTimeout = System.getProperty(COMPILE_DAEMON_STARTUP_TIMEOUT_PROPERTY)?.let {
                try {
                    it.toLong()
                }
                catch (e: Exception) {
                    reportingTargets.report(DaemonReportCategory.INFO, "unable to interpret $COMPILE_DAEMON_STARTUP_TIMEOUT_PROPERTY property ('$it'); using default timeout $DAEMON_DEFAULT_STARTUP_TIMEOUT_MS ms")
                    null
                }
            } ?: DAEMON_DEFAULT_STARTUP_TIMEOUT_MS
            if (daemonOptions.runFilesPath.isNotEmpty()) {
                val succeeded = isEchoRead.tryAcquire(daemonStartupTimeout, TimeUnit.MILLISECONDS)
                return when {
                    !isProcessAlive(daemon) -> {
                        reportingTargets.report(DaemonReportCategory.INFO, "Daemon terminated unexpectedly with error code: ${daemon.exitValue()}")
                        false
                    }
                    !succeeded -> {
                        reportingTargets.report(DaemonReportCategory.INFO, "Unable to get response from daemon in $daemonStartupTimeout ms")
                        false
                    }
                    else -> true
                }
            }
            else
            // without startEcho defined waiting for max timeout
                Thread.sleep(daemonStartupTimeout)
            return true
        }
        finally {
            // assuming that all important output is already done, the rest should be routed to the log by the daemon itself
            if (stdoutThread.isAlive) {
                // TODO: find better method to stop the thread, but seems it will require asynchronous consuming of the stream
                stdoutThread.stop()
            }
            reportingTargets.out?.flush()
        }
    }

    override fun main(vararg args: String) = oldKotlinCompilerClient.main(*args)

}

data class DaemonReportMessage(val category: DaemonReportCategory, val message: String)

class DaemonReportingTargets(
    val out: PrintStream? = null,
    val messages: MutableCollection<DaemonReportMessage>? = null,
    val messageCollector: MessageCollector? = null,
    val compilerServices: CompilerServicesFacadeBaseAsync? = null
)