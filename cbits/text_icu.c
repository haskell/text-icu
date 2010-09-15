#include "hs_text_icu.h"

UBreakIterator* __hs_ubrk_open(UBreakIteratorType type, const char *locale,
			       const UChar *text, int32_t textLength,
			       UErrorCode *status)
{
    return ubrk_open(type, locale, text, textLength, status);
}

void __hs_ubrk_close(UBreakIterator *bi)
{
    ubrk_close(bi);
}

void __hs_ubrk_setText(UBreakIterator* bi, const UChar *text,
		       int32_t textLength, UErrorCode *status)
{
    ubrk_setText(bi, text, textLength, status);
}

int32_t __hs_ubrk_first(UBreakIterator *bi)
{
    return ubrk_first(bi);
}

int32_t __hs_ubrk_last(UBreakIterator *bi)
{
    return ubrk_last(bi);
}

int32_t __hs_ubrk_next(UBreakIterator *bi)
{
    return ubrk_next(bi);
}

int32_t __hs_ubrk_previous(UBreakIterator *bi)
{
    return ubrk_previous(bi);
}

int32_t __hs_ubrk_getRuleStatus(UBreakIterator *bi)
{
    return ubrk_getRuleStatus(bi);
}

int32_t __hs_ubrk_getRuleStatusVec(UBreakIterator *bi, int32_t *fillInVec,
				   int32_t capacity, UErrorCode *status)
{
    return ubrk_getRuleStatusVec(bi, fillInVec, capacity, status);
}

UCollator* __hs_ucol_open(const char *loc, UErrorCode *status)
{
    return ucol_open(loc, status);
}

void __hs_ucol_close(UCollator *coll)
{
    ucol_close(coll);
}

void __hs_ucol_setAttribute(UCollator *coll, UColAttribute attr,
			    UColAttributeValue value, UErrorCode *status)
{
    ucol_setAttribute(coll, attr, value, status);
}

UColAttributeValue __hs_ucol_getAttribute(const UCollator *coll,
					  UColAttribute attr,
					  UErrorCode *status)
{
    return ucol_getAttribute(coll, attr, status);
}

UCollationResult __hs_ucol_strcoll(const UCollator *coll,
				   const UChar *source, int32_t sourceLength,
				   const UChar *target, int32_t targetLength)
{
    return ucol_strcoll(coll, source, sourceLength, target, targetLength);
}

UCollationResult __hs_ucol_strcollIter(const UCollator *coll,
				       UCharIterator *sIter,
				       UCharIterator *tIter,
				       UErrorCode *status)
{
    return ucol_strcollIter(coll, sIter, tIter, status);
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

UBool __hs_ucol_equals(const UCollator *source, const UCollator *target)
{
    return ucol_equals(source, target);
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

void __hs_uiter_setString(UCharIterator *iter, const UChar *s, int32_t length)
{
    uiter_setString(iter, s, length);
}

void __hs_uiter_setUTF8(UCharIterator *iter, const char *s, int32_t length)
{
    uiter_setUTF8(iter, s, length);
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

int32_t __hs_u_strToUpper(UChar *dest, int32_t destCapacity,
			  const UChar *src, int32_t srcLength,
			  const char *locale, UErrorCode *pErrorCode)
{
    return u_strToUpper(dest, destCapacity, src, srcLength, locale, pErrorCode);
}

int32_t __hs_u_strToLower(UChar *dest, int32_t destCapacity,
			  const UChar *src, int32_t srcLength,
			  const char *locale, UErrorCode *pErrorCode)
{
    return u_strToLower(dest, destCapacity, src, srcLength, locale, pErrorCode);
}

int32_t __hs_u_strFoldCase(UChar *dest, int32_t destCapacity,
			   const UChar *src, int32_t srcLength,
			   uint32_t options, UErrorCode *pErrorCode)
{
    return u_strFoldCase(dest, destCapacity, src, srcLength, options,
			 pErrorCode);
}

int32_t __hs_u_strCompareIter(UCharIterator *iter1, UCharIterator *iter2)
{
    return u_strCompareIter(iter1, iter2, TRUE);
}

UBlockCode __hs_ublock_getCode(UChar32 c)
{
    return ublock_getCode(c);
}

UCharDirection __hs_u_charDirection(UChar32 c)
{
    return u_charDirection(c);
}

UBool __hs_u_isMirrored(UChar32 c)
{
    return u_isMirrored(c);
}

UChar32 __hs_u_charMirror(UChar32 c)
{
    return u_charMirror(c);
}

uint8_t __hs_u_getCombiningClass(UChar32 c)
{
    return u_getCombiningClass(c);
}

int32_t __hs_u_charDigitValue(UChar32 c)
{
    return u_charDigitValue(c);
}

int32_t __hs_u_charName(UChar32 code, UCharNameChoice nameChoice,
			char *buffer, int32_t bufferLength,
			UErrorCode *pErrorCode)
{
    return u_charName(code, nameChoice, buffer, bufferLength, pErrorCode);
}

int32_t __hs_u_getISOComment(UChar32 c, char *dest, int32_t destCapacity,
			     UErrorCode *pErrorCode)
{
    return u_getISOComment(c, dest, destCapacity, pErrorCode);
}

UBool __hs_u_hasBinaryProperty(UChar32 c, UProperty which)
{
    return u_hasBinaryProperty(c, which);
}
