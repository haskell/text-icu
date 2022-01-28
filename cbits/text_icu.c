#include "hs_text_icu.h"
#include "stdbool.h"

UDateFormat *__hs_udat_open(UDateFormatStyle timeStyle, UDateFormatStyle dateStyle, const char *locale, const UChar *tzID, int32_t tzIDLength, const UChar *pattern, int32_t patternLength, UErrorCode *status)
{
    return udat_open(timeStyle, dateStyle, locale, tzID, tzIDLength, pattern, patternLength, status);
}

void __hs_udat_close(UDateFormat *format)
{
    udat_close(format);
}

UDateFormat *__hs_udat_clone(const UDateFormat *fmt, UErrorCode *status)
{
    return udat_clone(fmt, status);
}

int32_t __hs_udat_formatCalendar(const UDateFormat *format, UCalendar *calendar, UChar *result, int32_t capacity, UFieldPosition *position, UErrorCode *status)
{
    return udat_formatCalendar(format, calendar, result, capacity, position, status);
}

int32_t __hs_udat_getSymbols(const UDateFormat *fmt, UDateFormatSymbolType type, int32_t symbolIndex, UChar *result, int32_t resultLength, UErrorCode *status)
{
    return udat_getSymbols(fmt, type, symbolIndex, result, resultLength, status);
}

int32_t __hs_udat_countSymbols(const UDateFormat *fmt, UDateFormatSymbolType type)
{
    return udat_countSymbols(fmt, type);
}

UNumberFormatter *__hs_unumf_openForSkeletonAndLocale(const UChar *skeleton, int32_t skeletonLen, const char *locale, UErrorCode *ec)
{
    return unumf_openForSkeletonAndLocale(skeleton, skeletonLen, locale, ec);
}

void __hs_unumf_close(UNumberFormatter *uformatter)
{
    unumf_close(uformatter);
}

UFormattedNumber *__hs_unumf_openResult(UErrorCode *ec)
{
    return unumf_openResult(ec);
}

void __hs_unumf_closeResult(UFormattedNumber *uresult)
{
    unumf_closeResult(uresult);
}

void __hs_unumf_formatInt(const UNumberFormatter *uformatter, int64_t value, UFormattedNumber *uresult, UErrorCode *ec)
{
    unumf_formatInt(uformatter, value, uresult, ec);
}

void __hs_unumf_formatDouble(const UNumberFormatter *uformatter, double value, UFormattedNumber *uresult, UErrorCode *ec)
{
    unumf_formatDouble(uformatter, value, uresult, ec);
}

int32_t __hs_unumf_resultToString(const UFormattedNumber *uresult, UChar *buffer, int32_t bufferCapacity, UErrorCode *ec)
{
    return unumf_resultToString(uresult, buffer, bufferCapacity, ec);
}

const char *__hs_uloc_getAvailable(int32_t n)
{
    return uloc_getAvailable(n);
}

int32_t __hs_uloc_countAvailable(void)
{
    return uloc_countAvailable();
}

void __hs_uenum_close(UEnumeration *en)
{
    uenum_close(en);
}

const UChar *__hs_uenum_unext(UEnumeration *en, int32_t *resultLength, UErrorCode *status)
{
    return uenum_unext(en, resultLength, status);
}

UCalendar *__hs_ucal_open(const UChar *zoneID, int32_t len, const char *locale, UCalendarType type, UErrorCode *status)
{
    return ucal_open(zoneID, len, locale, type, status);
}

UCalendar *__hs_ucal_clone(const UCalendar *cal, UErrorCode *status)
{
    return ucal_clone(cal, status);
}

int32_t __hs_ucal_get(const UCalendar *cal, UCalendarDateFields field, UErrorCode *status)
{
    return ucal_get(cal, field, status);
}

void __hs_ucal_set(UCalendar *cal, UCalendarDateFields field, int32_t value)
{
    ucal_set(cal, field, value);
}

void __hs_ucal_setDate(UCalendar *cal, int32_t year, int32_t month, int32_t date, UErrorCode *status)
{
    __hs_ucal_setDate(cal, year, month, date, status);
}

void __hs_ucal_setDateTime(UCalendar *cal, int32_t year, int32_t month, int32_t date, int32_t hr, int32_t min, int32_t sec, UErrorCode *status)
{
    __hs_ucal_setDateTime(cal, year, month, date, hr, min, sec, status);
}

void __hs_ucal_add(UCalendar *cal, UCalendarDateFields field, int32_t value, UErrorCode *status)
{
    ucal_add(cal, field, value, status);
}

void __hs_ucal_roll(UCalendar *cal, UCalendarDateFields field, int32_t value, UErrorCode *status)
{
    ucal_roll(cal, field, value, status);
}

void __hs_ucal_close(UCalendar *cal)
{
    ucal_close(cal);
}

UEnumeration *_hs__ucal_openTimeZoneIDEnumeration(USystemTimeZoneType zoneType, UErrorCode *ec)
{
    return ucal_openTimeZoneIDEnumeration(zoneType, NULL, NULL, ec);
}

UEnumeration *__hs_ucal_openTimeZones(UErrorCode *ec)
{
    return ucal_openTimeZones(ec);
}

void __hs_ucal_setTimeZone(UCalendar *cal, const UChar *zoneID, int32_t len, UErrorCode *status)
{
    ucal_setTimeZone(cal, zoneID, len, status);
}


UBreakIterator *__hs_ubrk_open(UBreakIteratorType type, const char *locale,
                               const UChar *text, int32_t textLength,
                               UErrorCode *status)
{
    return ubrk_open(type, locale, text, textLength, status);
}

void __hs_ubrk_close(UBreakIterator *bi)
{
    ubrk_close(bi);
}

void __hs_ubrk_setUText(UBreakIterator* bi, UText *text,
                        UErrorCode *status)
{
    ubrk_setUText(bi, text, status);
}

UBreakIterator *__hs_ubrk_safeClone(const UBreakIterator *bi,
                                    void *stackBuffer, int32_t *pBufferSize,
                                    UErrorCode *status)
{
    return ubrk_safeClone(bi, stackBuffer, pBufferSize, status);
}

int32_t __hs_ubrk_current(const UBreakIterator *bi)
{
    return ubrk_current(bi);
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

int32_t __hs_ubrk_preceding(UBreakIterator *bi, int32_t offset)
{
    return ubrk_preceding(bi, offset);
}

int32_t __hs_ubrk_following(UBreakIterator *bi, int32_t offset)
{
    return ubrk_following(bi, offset);
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

UBool __hs_ubrk_isBoundary(UBreakIterator *bi, int32_t offset)
{
    return ubrk_isBoundary(bi, offset);
}

int32_t __hs_ubrk_countAvailable(void)
{
    return ubrk_countAvailable();
}

const char *__hs_ubrk_getAvailable(int32_t index)
{
    return ubrk_getAvailable(index);
}

UCollator *__hs_ucol_open(const char *loc, UErrorCode *status)
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

UCollationResult __hs_ucol_strcollUTF8(
    const UCollator *coll, const char *source, int32_t sourceLength,
    const char *target, int32_t targetLength, UErrorCode *status)
{
    return ucol_strcollUTF8(coll, source, sourceLength, target, targetLength, status);
}

UCollationResult __hs_ucol_strcollIter(const UCollator *coll,
                                       UCharIterator *sIter,
                                       UCharIterator *tIter,
                                       UErrorCode *status)
{
    return ucol_strcollIter(coll, sIter, tIter, status);
}

UCollator *__hs_ucol_safeClone(const UCollator *coll,
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

int __hs_ucnv_get_max_bytes_for_string(UConverter *cnv, int src_length)
{
    return UCNV_GET_MAX_BYTES_FOR_STRING(src_length, ucnv_getMaxCharSize(cnv));
}

const char *__hs_u_errorName(UErrorCode code)
{
    return u_errorName(code);
}


UBiDi* __hs_ubidi_open()
{
    return ubidi_open();
}

UBiDi* __hs_ubidi_openSized(int32_t maxLength, int32_t maxRunCount, UErrorCode *err)
{
    return ubidi_openSized(maxLength, maxRunCount, err);
}

void __hs_ubidi_close(UBiDi * bidi)
{
    ubidi_close(bidi);
}

void 	__hs_ubidi_setPara(UBiDi *pBiDi, const UChar *text, int32_t length, UBiDiLevel paraLevel,
                         UErrorCode *pErrorCode)
{
    ubidi_setPara(pBiDi, text, length, paraLevel, NULL, pErrorCode);
}

void __hs_ubidi_setLine(const UBiDi *pParaBiDi, int32_t start, int32_t limit,
                        UBiDi *pLineBiDi, UErrorCode *pErrorCode)
{
    ubidi_setLine(pParaBiDi, start, limit, pLineBiDi, pErrorCode);
}

int32_t __hs_ubidi_countParagraphs(UBiDi *pBiDi)
{
    return ubidi_countParagraphs(pBiDi);
}

void __hs_ubidi_getParagraphByIndex(const UBiDi *pBiDi, int32_t paraIndex, int32_t *pParaStart,
                                    int32_t *pParaLimit, UErrorCode *pErrorCode)
{
    ubidi_getParagraphByIndex(pBiDi, paraIndex, pParaStart, pParaLimit, NULL, pErrorCode);
}

int32_t __hs_ubidi_getProcessedLength(const UBiDi *pBiDi)
{
    return ubidi_getProcessedLength(pBiDi);
}

int32_t __hs_ubidi_writeReordered(UBiDi *pBiDi, UChar *dest, int32_t destSize, uint16_t options,
                              		UErrorCode *pErrorCode)
{
    return ubidi_writeReordered(pBiDi, dest, destSize, options, pErrorCode);
}

const char *__hs_ucnv_getName(const UConverter *converter, UErrorCode *err)
{
    return ucnv_getName(converter, err);
}

UConverter *__hs_ucnv_open(const char *converterName, UErrorCode *err)
{
    return ucnv_open(converterName, err);
}

void __hs_ucnv_close(UConverter *converter)
{
    ucnv_close(converter);
}

int32_t __hs_ucnv_toUChars(UConverter *cnv, UChar *dest, int32_t destCapacity,
                           const char *src, int32_t srcLength,
                           UErrorCode *pErrorCode)
{
    return ucnv_toUChars(cnv, dest, destCapacity, src, srcLength, pErrorCode);
}

int32_t __hs_ucnv_toAlgorithmic_UTF8(
    UConverter *cnv, char *dest, int32_t destCapacity,
    const char *src, int32_t srcLength,
    UErrorCode *pErrorCode)
{
    return ucnv_toAlgorithmic(UCNV_UTF8, cnv, dest, destCapacity, src, srcLength, pErrorCode);
}

int32_t __hs_ucnv_fromUChars(UConverter *cnv, char *dest, int32_t destCapacity,
                             const UChar *src, int32_t srcLength,
                             UErrorCode *pErrorCode)
{
    return ucnv_fromUChars(cnv, dest, destCapacity, src, srcLength, pErrorCode);
}

int32_t __hs_ucnv_fromAlgorithmic_UTF8(
    UConverter *cnv, char *dest, int32_t destCapacity,
    const char *src, int32_t srcLength,
    UErrorCode *pErrorCode)
{
    return ucnv_fromAlgorithmic(cnv, UCNV_UTF8, dest, destCapacity, src, srcLength, pErrorCode);
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

const char *__hs_ucnv_getAvailableName(int32_t n)
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

const UNormalizer2 *__hs_unorm2_getNFCInstance(UErrorCode *pErrorCode)
{
    return unorm2_getNFCInstance(pErrorCode);
}

const UNormalizer2 *__hs_unorm2_getNFDInstance(UErrorCode *pErrorCode)
{
    return unorm2_getNFDInstance(pErrorCode);
}

const UNormalizer2 *__hs_unorm2_getNFKCInstance(UErrorCode *pErrorCode)
{
    return unorm2_getNFKCInstance(pErrorCode);
}

const UNormalizer2 *__hs_unorm2_getNFKDInstance(UErrorCode *pErrorCode)
{
    return unorm2_getNFKDInstance(pErrorCode);
}

const UNormalizer2 *__hs_unorm2_getNFKCCasefoldInstance(UErrorCode *pErrorCode)
{
    return unorm2_getNFKCCasefoldInstance(pErrorCode);
}

int32_t __hs_unorm2_normalize(const UNormalizer2 *norm2, const UChar *src, int32_t length, UChar *dest, int32_t capacity, UErrorCode *pErrorCode)
{
    return unorm2_normalize(norm2, src, length, dest, capacity, pErrorCode);
}

UBool __hs_unorm2_isNormalized(const UNormalizer2 *norm2, const UChar *s, int32_t length, UErrorCode *pErrorCode)
{
    return unorm2_isNormalized(norm2, s, length, pErrorCode);
}

UNormalizationCheckResult __hs_unorm2_quickCheck(const UNormalizer2 *norm2, const UChar *s, int32_t length, UErrorCode *pErrorCode)
{
    return unorm2_quickCheck(norm2, s, length, pErrorCode);
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

int32_t __hs_u_shapeArabic(const UChar *source, int32_t sourceLength,
                           UChar *result, int32_t resultLength,
                           int32_t options, UErrorCode *status)
{
    return u_shapeArabic(source, sourceLength, result,
                         resultLength, options, status);
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
    return u_strCompareIter(iter1, iter2, true);
}

UChar* __hs_u_strFromUTF8Lenient(
    UChar *dest, int32_t destCapacity, int32_t *pDestLength,
    const char *src, int32_t srcLength, UErrorCode *pErrorCode)
{
    return u_strFromUTF8Lenient(dest, destCapacity, pDestLength,
                                src, srcLength, pErrorCode);
}

char* __hs_u_strToUTF8(
    char *dest, int32_t destCapacity, int32_t *pDestLength,
    const UChar *src, int32_t srcLength,
    UErrorCode *pErrorCode)
{
    return u_strToUTF8(dest, destCapacity, pDestLength, src, srcLength, pErrorCode);
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

UChar32 __hs_u_charFromName(UCharNameChoice nameChoice, const char *name,
                            UErrorCode *pErrorCode)
{
    return u_charFromName(nameChoice, name, pErrorCode);
}

int32_t __hs_u_getIntPropertyValue(UChar32 c, UProperty which)
{
    return u_getIntPropertyValue(c, which);
}

double __hs_u_getNumericValue(UChar32 c)
{
    return u_getNumericValue(c);
}

URegularExpression *__hs_uregex_open(const UChar *pattern,
                                     int32_t patternLength, uint32_t flags,
                                     UParseError *pe, UErrorCode *status)
{
    return uregex_open(pattern, patternLength, flags, pe, status);
}

void __hs_uregex_setTimeLimit(URegularExpression *regexp,
                              int32_t limit, UErrorCode *status)
{
    return uregex_setTimeLimit(regexp, limit, status);
}

void __hs_uregex_setStackLimit(URegularExpression *regexp,
                               int32_t limit, UErrorCode *status)
{
    return uregex_setStackLimit(regexp, limit, status);
}

void __hs_uregex_close(URegularExpression *regexp)
{
    return uregex_close(regexp);
}

URegularExpression *__hs_uregex_clone(const URegularExpression *regexp,
                                      UErrorCode *pErrorCode)
{
    return uregex_clone(regexp, pErrorCode);
}

const UChar *__hs_uregex_pattern(const URegularExpression *regexp,
                                 int32_t *patLength, UErrorCode *status)
{
    return uregex_pattern(regexp, patLength, status);
}

int32_t __hs_uregex_flags(const URegularExpression *regexp,
                          UErrorCode *status)
{
    return uregex_flags(regexp, status);
}

void __hs_uregex_setUText(URegularExpression *regexp, UText *text,
                          UErrorCode *status)
{
    return uregex_setUText(regexp, text, status);
}

const UChar *__hs_uregex_getText(URegularExpression *regexp,
                                 int32_t *textLength, UErrorCode *status)
{
    return uregex_getText(regexp, textLength, status);
}

UBool __hs_uregex_find(URegularExpression *regexp, int32_t startIndex,
                       UErrorCode *status)
{
    return uregex_find(regexp, startIndex, status);
}

UBool __hs_uregex_findNext(URegularExpression *regexp, UErrorCode *status)
{
    return uregex_findNext(regexp, status);
}

int32_t __hs_uregex_start(URegularExpression *regexp, int32_t groupNum,
                          UErrorCode *status)
{
    return uregex_start(regexp, groupNum, status);
}

int32_t __hs_uregex_end(URegularExpression *regexp, int32_t groupNum,
                        UErrorCode *status)
{
    return uregex_end(regexp, groupNum, status);
}

int32_t __hs_uregex_groupCount(URegularExpression *regexp, UErrorCode *status)
{
    return uregex_groupCount(regexp, status);
}

int32_t __hs_uregex_group(URegularExpression *regexp, int32_t groupNum,
                          UChar *dest, int32_t destCapacity,
                          UErrorCode *status)
{
    return uregex_group(regexp, groupNum, dest, destCapacity, status);
}

USpoofChecker *__hs_uspoof_open(UErrorCode *status)
{
    return uspoof_open(status);
}

USpoofChecker *__hs_uspoof_openFromSerialized(const void *data, int32_t length,
                                              int32_t *pActualLength,
                                              UErrorCode *status)
{
    return uspoof_openFromSerialized(data, length, pActualLength, status);
}

USpoofChecker *__hs_uspoof_openFromSource(const char *confusables,
                                          int32_t confusablesLen,
                                          const char *confusablesWholeScript,
                                          int32_t confusablesWholeScriptLen,
                                          int32_t *errType,
                                          UParseError *parseError,
                                          UErrorCode *status)
{
    // Work around missing call to umtx_initOnce() in uspoof_openFromSource()
    // causing crash when gNfdNormalizer is accessed
    uspoof_getInclusionSet(status);
    return uspoof_openFromSource(confusables, confusablesLen,
                                 confusablesWholeScript,
                                 confusablesWholeScriptLen,
                                 errType, parseError, status);
}

void __hs_uspoof_setChecks(USpoofChecker *sc, int32_t checks,
                           UErrorCode *status)
{
    uspoof_setChecks(sc, checks, status);
}

int32_t __hs_uspoof_getChecks(const USpoofChecker *sc, UErrorCode *status)
{
    return uspoof_getChecks(sc, status);
}

void __hs_uspoof_setRestrictionLevel(USpoofChecker *sc, URestrictionLevel level)
{
    uspoof_setRestrictionLevel(sc, level);
}

URestrictionLevel __hs_uspoof_getRestrictionLevel(const USpoofChecker *sc)
{
    return uspoof_getRestrictionLevel(sc);
}

void __hs_uspoof_setAllowedLocales(USpoofChecker *sc, const char *localesList,
                                   UErrorCode *status)
{
    uspoof_setAllowedLocales(sc, localesList, status);
}

const char *__hs_uspoof_getAllowedLocales(USpoofChecker *sc, UErrorCode *status)
{
    return uspoof_getAllowedLocales(sc, status);
}

int32_t __hs_uspoof_check(USpoofChecker *sc, const UChar *id,
                          int32_t length, int32_t *position,
                          UErrorCode *status)
{
    return uspoof_check(sc, id, length, position, status);
}

int32_t __hs_uspoof_areConfusable(USpoofChecker *sc,
                                  const UChar *id1, int32_t length1,
                                  const UChar *id2, int32_t length2,
                                  UErrorCode *status)
{
    return uspoof_areConfusable(sc, id1, length1, id2, length2, status);
}

int32_t __hs_uspoof_getSkeleton(USpoofChecker *sc,
                                int32_t type, const UChar *id, int32_t length,
                                UChar *dest, int32_t destCapacity,
                                UErrorCode *status)
{
    return uspoof_getSkeleton(sc, type, id, length, dest, destCapacity, status);
}

int32_t __hs_uspoof_checkUTF8(
    USpoofChecker *sc, const char *id,
    int32_t length, int32_t *position,
    UErrorCode *status)
{
    return uspoof_checkUTF8(sc, id, length, position, status);
}

int32_t __hs_uspoof_areConfusableUTF8(
    USpoofChecker *sc,
    const char *id1, int32_t length1,
    const char *id2, int32_t length2,
    UErrorCode *status)
{
    return uspoof_areConfusableUTF8(sc, id1, length1, id2, length2, status);
}

int32_t __hs_uspoof_getSkeletonUTF8(
    USpoofChecker *sc,
    int32_t type, const char *id, int32_t length,
    char *dest, int32_t destCapacity,
    UErrorCode *status)
{
    return uspoof_getSkeletonUTF8(sc, type, id, length, dest, destCapacity, status);
}

int32_t __hs_uspoof_serialize(USpoofChecker *sc, void *data, int32_t capacity,
                              UErrorCode *status)
{
    return uspoof_serialize(sc, data, capacity, status);
}

void __hs_uspoof_close(USpoofChecker *sc)
{
    uspoof_close(sc);
}

UText* __hs_utext_openUChars(UText *ut, const UChar *s, int64_t length,
                             UErrorCode * status)
{
    return utext_openUChars(ut, s, length, status);
}
UText* __hs_utext_openUTF8(UText *ut, const char *s, int64_t length,
                           UErrorCode * status)
{
    return utext_openUTF8(ut, s, length, status);
}
void __hs_utext_close(UText *ut)
{
    utext_close(ut);
}

UCharsetDetector *__hs_ucsdet_open(UErrorCode *status)
{
    return ucsdet_open(status);
}

void __hs_ucsdet_close(UCharsetDetector *ucsd)
{
    ucsdet_close(ucsd);
}

void __hs_ucsdet_setText(UCharsetDetector *ucsd,
                         const char *textIn, int32_t length,
                         UErrorCode *status)
{
    ucsdet_setText(ucsd, textIn, length, status);
}

void __hs_ucsdet_setDeclaredEncoding(UCharsetDetector *ucsd,
                                     const char *encoding, int32_t length,
                                     UErrorCode *status)
{
    ucsdet_setDeclaredEncoding(ucsd, encoding, length, status);
}

const UCharsetMatch *__hs_ucsdet_detect(UCharsetDetector *ucsd, UErrorCode *status)
{
    return ucsdet_detect(ucsd, status);
}

const UCharsetMatch **__hs_ucsdet_detectAll(UCharsetDetector *ucsd,
                                            int32_t *matchesFound,
                                            UErrorCode *status)
{
    return ucsdet_detectAll(ucsd, matchesFound, status);
}

const char *__hs_ucsdet_getName(const UCharsetMatch *ucsm,
                                UErrorCode *status)
{
    return ucsdet_getName(ucsm, status);
}

int32_t __hs_ucsdet_getConfidence(const UCharsetMatch *ucsm,
                                  UErrorCode *status)
{
    return ucsdet_getConfidence(ucsm, status);
}

const char *__hs_ucsdet_getLanguage(const UCharsetMatch *ucsm,
                                    UErrorCode *status)
{
    return ucsdet_getLanguage(ucsm, status);
}

int32_t __hs_ucsdet_getUChars(const UCharsetMatch *ucsm,
                              UChar *buf, int32_t capacity,
                              UErrorCode *status)
{
    return ucsdet_getUChars(ucsm, buf, capacity, status);
}

UEnumeration *__hs_ucsdet_getAllDetectableCharsets(const UCharsetDetector *ucsd,
                                                   UErrorCode *status)
{
    return ucsdet_getAllDetectableCharsets(ucsd, status);
}

UBool __hs_ucsdet_isInputFilterEnabled(const UCharsetDetector *ucsd)
{
    return ucsdet_isInputFilterEnabled(ucsd);
}

UBool __hs_ucsdet_enableInputFilter(UCharsetDetector *ucsd, UBool filter)
{
    return ucsdet_enableInputFilter(ucsd, filter);
}

UEnumeration *__hs_ucsdet_getDetectableCharsets(const UCharsetDetector *ucsd,
                                                UErrorCode *status)
{
    return ucsdet_getDetectableCharsets(ucsd, status);
}

void __hs_ucsdet_setDetectableCharset(UCharsetDetector *ucsd,
                                      const char *encoding, UBool enabled,
                                      UErrorCode *status)
{
    return ucsdet_setDetectableCharset(ucsd, encoding, enabled, status);
}

UNumberFormat *__hs_unum_open(UNumberFormatStyle style, const UChar *pattern, int32_t patternLength, const char *loc, UParseError *parseErr, UErrorCode *status)
{
    if (patternLength == 0)
        return unum_open(style, 0, 0, loc, parseErr, status);
    else
        return unum_open(style, pattern, patternLength, loc, parseErr, status);
}

void __hs_unum_close(UNumberFormat *fmt)
{
    unum_close(fmt);
}

int32_t __hs_unum_formatInt64(const UNumberFormat *fmt, int64_t value, UChar *result, int32_t resultLength, UErrorCode *ec)
{
    return unum_formatInt64(fmt, value, result, resultLength, 0, ec);
}

int32_t __hs_unum_formatDouble(const UNumberFormat *fmt, double value, UChar *result, int32_t resultLength, UErrorCode *ec)
{
    return unum_formatDouble(fmt, value, result, resultLength, 0, ec);
}
