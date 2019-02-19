/*
 * Copyright 2010-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.fir.java

import org.jetbrains.kotlin.descriptors.annotations.Annotations
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.expressions.FirAnnotationCall
import org.jetbrains.kotlin.fir.expressions.resolvedFqName
import org.jetbrains.kotlin.fir.expressions.toResolvedCallableSymbol
import org.jetbrains.kotlin.load.java.*
import org.jetbrains.kotlin.load.java.typeEnhancement.NullabilityQualifier
import org.jetbrains.kotlin.load.java.typeEnhancement.NullabilityQualifierWithMigrationStatus
import org.jetbrains.kotlin.utils.Jsr305State
import org.jetbrains.kotlin.utils.addToStdlib.firstNotNullResult

class FirSignatureEnhancement(private val session: FirSession, private val jsr305State: Jsr305State) {

    private fun FirAnnotationCall.extractNullabilityTypeFromArgument(): NullabilityQualifierWithMigrationStatus? {
        val enumValue = this.arguments.firstOrNull()?.toResolvedCallableSymbol()?.callableId?.callableName
        // if no argument is specified, use default value: NOT_NULL
            ?: return NullabilityQualifierWithMigrationStatus(NullabilityQualifier.NOT_NULL)

        return when (enumValue.asString()) {
            "ALWAYS" -> NullabilityQualifierWithMigrationStatus(NullabilityQualifier.NOT_NULL)
            "MAYBE", "NEVER" -> NullabilityQualifierWithMigrationStatus(NullabilityQualifier.NULLABLE)
            "UNKNOWN" -> NullabilityQualifierWithMigrationStatus(NullabilityQualifier.FORCE_FLEXIBILITY)
            else -> null
        }
    }

    fun List<FirAnnotationCall>.extractNullability(
        annotationTypeQualifierResolver: FirAnnotationTypeQualifierResolver
    ): NullabilityQualifierWithMigrationStatus? =
        this.firstNotNullResult { annotationCall ->
            this@FirSignatureEnhancement.extractNullability(
                annotationTypeQualifierResolver,
                annotationCall
            )
        }


    fun extractNullability(
        annotationTypeQualifierResolver: FirAnnotationTypeQualifierResolver,
        annotationCall: FirAnnotationCall
    ): NullabilityQualifierWithMigrationStatus? {
        extractNullabilityFromKnownAnnotations(annotationCall)?.let { return it }

        val typeQualifierAnnotation =
            annotationTypeQualifierResolver.resolveTypeQualifierAnnotation(annotationCall)
                ?: return null

        val jsr305State = annotationTypeQualifierResolver.resolveJsr305AnnotationState(annotationCall)
        if (jsr305State.isIgnore) return null

        return extractNullabilityFromKnownAnnotations(typeQualifierAnnotation)?.copy(isForWarningOnly = jsr305State.isWarning)
    }

    private fun extractNullabilityFromKnownAnnotations(
        annotationCall: FirAnnotationCall
    ): NullabilityQualifierWithMigrationStatus? {
        val annotationFqName = annotationCall.resolvedFqName ?: return null

        return when {
            annotationFqName in NULLABLE_ANNOTATIONS -> NullabilityQualifierWithMigrationStatus(NullabilityQualifier.NULLABLE)
            annotationFqName in NOT_NULL_ANNOTATIONS -> NullabilityQualifierWithMigrationStatus(NullabilityQualifier.NOT_NULL)
            annotationFqName == JAVAX_NONNULL_ANNOTATION -> annotationCall.extractNullabilityTypeFromArgument()

            annotationFqName == COMPATQUAL_NULLABLE_ANNOTATION && jsr305State.enableCompatqualCheckerFrameworkAnnotations ->
                NullabilityQualifierWithMigrationStatus(NullabilityQualifier.NULLABLE)

            annotationFqName == COMPATQUAL_NONNULL_ANNOTATION && jsr305State.enableCompatqualCheckerFrameworkAnnotations ->
                NullabilityQualifierWithMigrationStatus(NullabilityQualifier.NOT_NULL)

            annotationFqName == ANDROIDX_RECENTLY_NON_NULL_ANNOTATION -> NullabilityQualifierWithMigrationStatus(
                NullabilityQualifier.NOT_NULL,
                isForWarningOnly = true
            )

            annotationFqName == ANDROIDX_RECENTLY_NULLABLE_ANNOTATION -> NullabilityQualifierWithMigrationStatus(
                NullabilityQualifier.NULLABLE,
                isForWarningOnly = true
            )
            else -> null
        }
    }
}