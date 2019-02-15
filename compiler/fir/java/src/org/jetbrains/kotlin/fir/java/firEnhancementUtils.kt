/*
 * Copyright 2010-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.fir.java

import org.jetbrains.kotlin.fir.types.*
import org.jetbrains.kotlin.fir.types.impl.FirResolvedTypeRefImpl
import org.jetbrains.kotlin.load.java.AnnotationTypeQualifierResolver
import org.jetbrains.kotlin.load.java.structure.JavaType
import org.jetbrains.kotlin.load.java.typeEnhancement.JavaTypeQualifiers
import org.jetbrains.kotlin.load.java.typeEnhancement.TypeComponentPosition
import org.jetbrains.kotlin.load.java.typeEnhancement.shouldEnhance

private data class TypeAndDefaultQualifiers(
    val type: FirResolvedTypeRef,
    val defaultQualifiers: JavaTypeQualifiers?
)

private fun FirResolvedTypeRef.typeArguments(): List<FirTypeProjection> =
    (this as? FirUserTypeRef)?.qualifier?.lastOrNull()?.typeArguments.orEmpty()

private fun FirResolvedTypeRef.toIndexed(
    typeQualifierResolver: FirAnnotationTypeQualifierResolver,
    signatureEnhancement: FirSignatureEnhancement,
    context: FirJavaEnhancementContext
): List<TypeAndDefaultQualifiers> {
    val list = ArrayList<TypeAndDefaultQualifiers>(1)

    fun add(type: FirResolvedTypeRef) {
        val c = context.copyWithNewDefaultTypeQualifiers(typeQualifierResolver, signatureEnhancement, type.annotations)

        list.add(
            TypeAndDefaultQualifiers(
                type,
                c.defaultTypeQualifiers
                    ?.get(AnnotationTypeQualifierResolver.QualifierApplicabilityType.TYPE_USE)
            )
        )

        for (arg in type.typeArguments()) {
            if (arg is FirStarProjection) {
                // TODO: wildcards
                // TODO: sort out how to handle wildcards
                //list.add(TypeAndDefaultQualifiers(arg.type))
            } else if (arg is FirTypeProjectionWithVariance) {
                add(arg.typeRef as FirResolvedTypeRef)
            }
        }
    }

    add(this)
    return list
}


internal fun FirResolvedTypeRef.computeIndexedQualifiers(): (Int) -> JavaTypeQualifiers {
    return { JavaTypeQualifiers.NONE }
}

internal fun FirResolvedTypeRef.enhanceReturnType(
    javaType: JavaType,
    qualifiers: (Int) -> JavaTypeQualifiers = computeIndexedQualifiers()
): FirResolvedTypeRef {
    return enhanceType(javaType, qualifiers, 0)
}

internal fun FirResolvedTypeRef.enhanceParameterType(
    javaType: JavaType,
    qualifiers: (Int) -> JavaTypeQualifiers = computeIndexedQualifiers()
): FirResolvedTypeRef {
    return enhanceType(javaType, qualifiers, 0)
}

// The index in the lambda is the position of the type component:
// Example: for `A<B, C<D, E>>`, indices go as follows: `0 - A<...>, 1 - B, 2 - C<D, E>, 3 - D, 4 - E`,
// which corresponds to the left-to-right breadth-first walk of the tree representation of the type.
// For flexible types, both bounds are indexed in the same way: `(A<B>..C<D>)` gives `0 - (A<B>..C<D>), 1 - B and D`.
private fun FirResolvedTypeRef.enhanceType(javaType: JavaType, qualifiers: (Int) -> JavaTypeQualifiers, index: Int): FirResolvedTypeRef {
    val type = type
    if (type is ConeKotlinErrorType || type is ConeClassErrorType) return this
    return when (type) {
        is ConeFlexibleType -> {
            val lowerBound = type.lowerBound
            val enhancedLowerBound = enhanceInflexibleType(lowerBound, javaType, TypeComponentPosition.FLEXIBLE_LOWER, qualifiers, index)
            val upperBound = type.upperBound
            val enhancedUpperBound = enhanceInflexibleType(upperBound, javaType, TypeComponentPosition.FLEXIBLE_UPPER, qualifiers, index)
            if (enhancedLowerBound === lowerBound && enhancedUpperBound === upperBound) {
                this
            } else {
                FirResolvedTypeRefImpl(
                    session, psi,
                    ConeFlexibleType(enhancedLowerBound, enhancedUpperBound),
                    isMarkedNullable, annotations
                )
            }
        }
        else -> {
            val enhanced = enhanceInflexibleType(type, javaType, TypeComponentPosition.INFLEXIBLE, qualifiers, index)
            if (enhanced === type) {
                this
            } else {
                FirResolvedTypeRefImpl(session, psi, enhanced, isMarkedNullable, annotations)
            }
        }
    }
}

private fun FirResolvedTypeRef.enhanceInflexibleType(
    type: ConeKotlinType,
    javaType: JavaType,
    position: TypeComponentPosition,
    qualifiers: (Int) -> JavaTypeQualifiers,
    index: Int
): ConeKotlinType {
    val shouldEnhance = position.shouldEnhance()
    if (!shouldEnhance && type.typeArguments.isEmpty()) return type




    return type // TODO
}

