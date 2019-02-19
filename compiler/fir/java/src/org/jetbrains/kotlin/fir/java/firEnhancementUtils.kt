/*
 * Copyright 2010-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.fir.java

import org.jetbrains.kotlin.builtins.jvm.JavaToKotlinClassMap
import org.jetbrains.kotlin.fir.expressions.FirAnnotationCall
import org.jetbrains.kotlin.fir.expressions.FirAnnotationContainer
import org.jetbrains.kotlin.fir.expressions.resolvedFqName
import org.jetbrains.kotlin.fir.symbols.ConeClassLikeSymbol
import org.jetbrains.kotlin.fir.types.*
import org.jetbrains.kotlin.fir.types.impl.FirResolvedTypeRefImpl
import org.jetbrains.kotlin.load.java.AnnotationTypeQualifierResolver
import org.jetbrains.kotlin.load.java.MUTABLE_ANNOTATIONS
import org.jetbrains.kotlin.load.java.READ_ONLY_ANNOTATIONS
import org.jetbrains.kotlin.load.java.structure.JavaType
import org.jetbrains.kotlin.load.java.typeEnhancement.*
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.FqNameUnsafe
import org.jetbrains.kotlin.types.typeUtil.isTypeParameter

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

private fun ConeKotlinType.toFqNameUnsafe(): FqNameUnsafe? =
    ((this as? ConeSymbolBasedType)?.symbol as? ConeClassLikeSymbol)?.classId?.asSingleFqName()?.toUnsafe()

private fun FirResolvedTypeRef.extractQualifiers(): JavaTypeQualifiers {
    val type = type
    val (lower, upper) =
        if (type is ConeFlexibleType) {
            Pair(type.lowerBound, type.upperBound)
        } else {
            Pair(type, type)
        }

    val mapping = JavaToKotlinClassMap
    return JavaTypeQualifiers(
        when {
            lower.isMarkedNullable -> NullabilityQualifier.NULLABLE
            !upper.isMarkedNullable -> NullabilityQualifier.NOT_NULL
            else -> null
        },
        when {
            mapping.isReadOnly(lower.toFqNameUnsafe()) -> MutabilityQualifier.READ_ONLY
            mapping.isMutable(upper.toFqNameUnsafe()) -> MutabilityQualifier.MUTABLE
            else -> null
        },
        isNotNullTypeParameter = false //TODO: unwrap() is NotNullTypeParameter
    )
}

private fun composeAnnotations(first: List<FirAnnotationCall>, second: List<FirAnnotationCall>): List<FirAnnotationCall> {
    return when {
        first.isEmpty() -> second
        second.isEmpty() -> first
        else -> first + second
    }
}

private fun FirResolvedTypeRef.extractQualifiersFromAnnotations(
    typeContainer: FirAnnotationContainer?,
    isHeadTypeConstructor: Boolean,
    defaultQualifiersForType: JavaTypeQualifiers?,
    containerContext: FirJavaEnhancementContext,
    signatureEnhancement: FirSignatureEnhancement,
    typeQualifierResolver: FirAnnotationTypeQualifierResolver,
    containerApplicabilityType: AnnotationTypeQualifierResolver.QualifierApplicabilityType
): JavaTypeQualifiers {
    val composedAnnotation =
        if (isHeadTypeConstructor && typeContainer != null)
            composeAnnotations(typeContainer.annotations, annotations)
        else
            annotations

    fun <T : Any> List<FqName>.ifPresent(qualifier: T) =
        if (any { fqName -> composedAnnotation.any { it.resolvedFqName == fqName }}) qualifier else null

    fun <T : Any> uniqueNotNull(x: T?, y: T?) = if (x == null || y == null || x == y) x ?: y else null

    val defaultTypeQualifier =
        if (isHeadTypeConstructor)
            containerContext.defaultTypeQualifiers?.get(containerApplicabilityType)
        else
            defaultQualifiersForType

    val nullabilityInfo = with(signatureEnhancement) {
        composedAnnotation.extractNullability(typeQualifierResolver)
            ?: defaultTypeQualifier?.nullability?.let { nullability ->
                NullabilityQualifierWithMigrationStatus(
                    nullability,
                    defaultTypeQualifier.isNullabilityQualifierForWarning
                )
            }
    }

    return JavaTypeQualifiers(
        nullabilityInfo?.qualifier,
        uniqueNotNull(
            READ_ONLY_ANNOTATIONS.ifPresent(
                MutabilityQualifier.READ_ONLY
            ),
            MUTABLE_ANNOTATIONS.ifPresent(
                MutabilityQualifier.MUTABLE
            )
        ),
        isNotNullTypeParameter = nullabilityInfo?.qualifier == NullabilityQualifier.NOT_NULL && true, /* TODO: isTypeParameter()*/
        isNullabilityQualifierForWarning = nullabilityInfo?.isForWarningOnly == true
    )
}

private fun FirResolvedTypeRef.unwrapEnhancement(): FirResolvedTypeRef = this // TODO

private fun FirResolvedTypeRef.computeQualifiersForOverride(
    fromSupertypes: Collection<FirResolvedTypeRef>,
    defaultQualifiersForType: JavaTypeQualifiers?,
    isHeadTypeConstructor: Boolean,
    isCovariant: Boolean
): JavaTypeQualifiers {
    val superQualifiers = fromSupertypes.map { it.extractQualifiers() }
    val mutabilityFromSupertypes = superQualifiers.mapNotNull { it.mutability }.toSet()
    val nullabilityFromSupertypes = superQualifiers.mapNotNull { it.nullability }.toSet()
    val nullabilityFromSupertypesWithWarning = fromSupertypes
        .mapNotNull { it.unwrapEnhancement().extractQualifiers().nullability }
        .toSet()

    val own = extractQualifiersFromAnnotations(isHeadTypeConstructor, defaultQualifiersForType)
    val ownNullability = own.takeIf { !it.isNullabilityQualifierForWarning }?.nullability
    val ownNullabilityForWarning = own.nullability

    val isCovariantPosition = isCovariant && isHeadTypeConstructor
    val nullability =
        nullabilityFromSupertypes.select(ownNullability, isCovariantPosition)
            // Vararg value parameters effectively have non-nullable type in Kotlin
            // and having nullable types in Java may lead to impossibility of overriding them in Kotlin
            ?.takeUnless { isForVarargParameter && isHeadTypeConstructor && it == NullabilityQualifier.NULLABLE }

    val mutability =
        mutabilityFromSupertypes
            .select(MutabilityQualifier.MUTABLE, MutabilityQualifier.READ_ONLY, own.mutability, isCovariantPosition)

    val canChange = ownNullabilityForWarning != ownNullability || nullabilityFromSupertypesWithWarning != nullabilityFromSupertypes
    val isAnyNonNullTypeParameter = own.isNotNullTypeParameter || superQualifiers.any { it.isNotNullTypeParameter }
    if (nullability == null && canChange) {
        val nullabilityWithWarning =
            nullabilityFromSupertypesWithWarning.select(ownNullabilityForWarning, isCovariantPosition)

        return createJavaTypeQualifiers(
            nullabilityWithWarning, mutability,
            forWarning = true, isAnyNonNullTypeParameter = isAnyNonNullTypeParameter
        )
    }

    return createJavaTypeQualifiers(
        nullability, mutability,
        forWarning = nullability == null,
        isAnyNonNullTypeParameter = isAnyNonNullTypeParameter
    )
}



internal fun FirResolvedTypeRef.computeIndexedQualifiers(
    typeQualifierResolver: FirAnnotationTypeQualifierResolver,
    signatureEnhancement: FirSignatureEnhancement,
    context: FirJavaEnhancementContext,
    fromOverridden: Collection<FirResolvedTypeRef>,
    isCovariant: Boolean
): (Int) -> JavaTypeQualifiers {
    val indexedFromSupertypes = fromOverridden.map { it.toIndexed(typeQualifierResolver, signatureEnhancement, context) }
    val indexedThisType = toIndexed(typeQualifierResolver, signatureEnhancement, context)

    // The covariant case may be hard, e.g. in the superclass the return may be Super<T>, but in the subclass it may be Derived, which
    // is declared to extend Super<T>, and propagating data here is highly non-trivial, so we only look at the head type constructor
    // (outermost type), unless the type in the subclass is interchangeable with the all the types in superclasses:
    // e.g. we have (Mutable)List<String!>! in the subclass and { List<String!>, (Mutable)List<String>! } from superclasses
    // Note that `this` is flexible here, so it's equal to it's bounds
    val onlyHeadTypeConstructor = isCovariant && fromOverridden.any { true /*equalTypes(it, this)*/ }

    val treeSize = if (onlyHeadTypeConstructor) 1 else indexedThisType.size
    val computedResult = Array(treeSize) { index ->
        val isHeadTypeConstructor = index == 0
        assert(isHeadTypeConstructor || !onlyHeadTypeConstructor) { "Only head type constructors should be computed" }

        val (qualifiers, defaultQualifiers) = indexedThisType[index]
        val verticalSlice = indexedFromSupertypes.mapNotNull { it.getOrNull(index)?.type }

        // Only the head type constructor is safely co-variant
        qualifiers.computeQualifiersForOverride(verticalSlice, defaultQualifiers, isHeadTypeConstructor)
    }

    return { index -> computedResult.getOrElse(index) { JavaTypeQualifiers.NONE } }
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

