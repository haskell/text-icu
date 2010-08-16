#include "hs_text_icu.h"

UCollator* __hs_ucol_open(const char *loc, UErrorCode *status)
{
    return ucol_open(loc, status);
}

void __hs_ucol_close(UCollator *coll)
{
    ucol_close(coll);
}

UCollationResult __hs_ucol_strcoll(const UCollator *coll,
				   const UChar *source, int32_t sourceLength,
				   const UChar *target, int32_t targetLength)
{
    return ucol_strcoll(coll, source, sourceLength, target, targetLength);
}

UCollator* __hs_ucol_safeClone(const UCollator *coll,
			       void *stackBuffer,
			       int32_t *pBufferSize,
			       UErrorCode *status)
{
    return ucol_safeClone(coll, stackBuffer, pBufferSize, status);
}

int32_t __hs_ucol_getSortKey(const UCollator *coll,
			     const UChar *source, int32_t sourceLength,
			     uint8_t *result, int32_t resultLength)
{
    return ucol_getSortKey(coll, source, sourceLength, result, resultLength);
}

int __get_max_bytes_for_string(UConverter *cnv, int src_length)
{
    return UCNV_GET_MAX_BYTES_FOR_STRING(src_length, ucnv_getMaxCharSize(cnv));
}

const char *__hs_u_errorName(UErrorCode code)
{
    return u_errorName(code);
}

const char *__hs_ucnv_getName(const UConverter *converter, UErrorCode *err)
{
    return ucnv_getName(converter, err);
}

UConverter* __hs_ucnv_open(const char *converterName, UErrorCode *err)
{
    return ucnv_open(converterName, err);
}

void __hs_ucnv_close(UConverter * converter)
{
    ucnv_close(converter);
}

int32_t __hs_ucnv_toUChars(UConverter *cnv, UChar *dest, int32_t destCapacity,
			   const char *src, int32_t srcLength,
			   UErrorCode *pErrorCode)
{
    return ucnv_toUChars(cnv, dest, destCapacity, src, srcLength, pErrorCode);
}

int32_t __hs_ucnv_fromUChars(UConverter *cnv, char *dest, int32_t destCapacity,
			     const UChar *src, int32_t srcLength,
			     UErrorCode *pErrorCode)
{
    return ucnv_fromUChars(cnv, dest, destCapacity, src, srcLength, pErrorCode);
}

int __hs_ucnv_compareNames(const char *name1, const char *name2)
{
    return ucnv_compareNames(name1, name2);
}

const char *__hs_ucnv_getDefaultName(void)
{
    return ucnv_getDefaultName();
}

void __hs_ucnv_setDefaultName(const char *name)
{
    ucnv_setDefaultName(name);
}

int32_t __hs_ucnv_countAvailable(void)
{
    return ucnv_countAvailable();
}

const char* __hs_ucnv_getAvailableName(int32_t n)
{
    return ucnv_getAvailableName(n);
}

uint16_t __hs_ucnv_countAliases(const char *alias, UErrorCode *pErrorCode)
{
    return ucnv_countAliases(alias, pErrorCode);
}

const char *__hs_ucnv_getAlias(const char *alias, uint16_t n,
			       UErrorCode *pErrorCode)
{
    return ucnv_getAlias(alias, n, pErrorCode);
}

uint16_t __hs_ucnv_countStandards(void)
{
    return ucnv_countStandards();
}

const char *__hs_ucnv_getStandard(uint16_t n, UErrorCode *pErrorCode)
{
    return ucnv_getStandard(n, pErrorCode);
}

UBool __hs_ucnv_usesFallback(const UConverter *cnv)
{
    return ucnv_usesFallback(cnv);
}

void __hs_ucnv_setFallback(UConverter *cnv, UBool usesFallback)
{
    ucnv_setFallback(cnv, usesFallback);
}

UBool __hs_ucnv_isAmbiguous(const UConverter *cnv)
{
    return ucnv_isAmbiguous(cnv);
}

int32_t __hs_unorm_compare(const UChar *s1, int32_t length1,
			   const UChar *s2, int32_t length2,
			   uint32_t options,
			   UErrorCode *pErrorCode)
{
    return unorm_compare(s1, length1, s2, length2, options, pErrorCode);
}

UNormalizationCheckResult __hs_unorm_quickCheck(const UChar *source,
						int32_t sourcelength,
						UNormalizationMode mode,
						UErrorCode *status)
{
    return unorm_quickCheck(source, sourcelength, mode, status);
}

UBool __hs_unorm_isNormalized(const UChar *src, int32_t srcLength,
			      UNormalizationMode mode,
			      UErrorCode *pErrorCode)
{
    return unorm_isNormalized(src, srcLength, mode, pErrorCode);
}

int32_t __hs_unorm_normalize(const UChar *source, int32_t sourceLength,
			     UNormalizationMode mode, int32_t options,
			     UChar *result, int32_t resultLength,
			     UErrorCode *status)
{
    return unorm_normalize(source, sourceLength, mode, options, result,
			   resultLength, status);
}
