#ifdef WIN32
#define U_HAVE_INTTYPES_H 1
#endif

#include "unicode/utypes.h"

#include "unicode/ubrk.h"
#include "unicode/ucal.h"
#include "unicode/uchar.h"
#include "unicode/ucnv.h"
#include "unicode/ucol.h"
#include "unicode/uenum.h"
#include "unicode/uiter.h"
#include "unicode/uloc.h"
#include "unicode/unorm.h"
#include "unicode/unumberformatter.h"
#include "unicode/uregex.h"
#include "unicode/uspoof.h"
#include "unicode/ustring.h"

#include <stdint.h>

/* unumberformatter.h */

UNumberFormatter *__hs_unumf_openForSkeletonAndLocale(const UChar *skeleton, int32_t skeletonLen, const char *locale, UErrorCode *ec);
void __hs_unumf_close(UNumberFormatter *uformatter);
UFormattedNumber *__hs_unumf_openResult(UErrorCode *ec);
void __hs_unumf_closeResult(UFormattedNumber *uresult);
void __hs_unumf_formatInt(const UNumberFormatter *uformatter, int64_t value, UFormattedNumber *uresult, UErrorCode *ec);
void __hs_unumf_formatDouble(const UNumberFormatter *uformatter, double value, UFormattedNumber *uresult, UErrorCode *ec);
int32_t __hs_unumf_resultToString(const UFormattedNumber *uresult, UChar *buffer, int32_t bufferCapacity, UErrorCode *ec);

/* uenum.h */

void __hs_uenum_close(UEnumeration *en);
const UChar *__hs_uenum_unext(UEnumeration *en, int32_t *resultLength, UErrorCode *status);

/* uloc.h */

const char *__hs_uloc_getAvailable(int32_t n);
int32_t __hs_uloc_countAvailable(void);

/* ucal.h */

UCalendar *__hs_ucal_open(const UChar *zoneID, int32_t len, const char *locale, UCalendarType type, UErrorCode *status);
UCalendar *__hs_ucal_clone(const UCalendar *cal, UErrorCode *status);
int32_t __hs_ucal_get(const UCalendar *cal, UCalendarDateFields field, UErrorCode *status);
void __hs_ucal_set(UCalendar *cal, UCalendarDateFields field, int32_t value);
void __hs_ucal_setDate(UCalendar *cal, int32_t year, int32_t month, int32_t date, UErrorCode *status);
void __hs_ucal_setDateTime(UCalendar *cal, int32_t year, int32_t month, int32_t date, int32_t hr, int32_t min, int32_t sec, UErrorCode *status);
void __hs_ucal_add(UCalendar *cal, UCalendarDateFields field, int32_t value, UErrorCode *status);
void __hs_ucal_roll(UCalendar *cal, UCalendarDateFields field, int32_t value, UErrorCode *status);
UEnumeration *__hs_ucal_openTimeZones(UErrorCode *ec);
UEnumeration *__hs_ucal_openTimeZoneIDEnumeration(USystemTimeZoneType zoneType, UErrorCode *ec);
void __hs_ucal_setTimeZone(UCalendar *cal, const UChar *zoneID, int32_t len, UErrorCode *status);

/* ubrk.h */

UBreakIterator *__hs_ubrk_open(UBreakIteratorType type, const char *locale,
							   const UChar *text, int32_t textLength,
							   UErrorCode *status);
void __hs_ubrk_close(UBreakIterator *bi);
void __hs_ubrk_setText(UBreakIterator *bi, const UChar *text,
					   int32_t textLength, UErrorCode *status);
UBreakIterator *__hs_ubrk_safeClone(const UBreakIterator *bi,
									void *stackBuffer, int32_t *pBufferSize,
									UErrorCode *status);
int32_t __hs_ubrk_current(const UBreakIterator *bi);
int32_t __hs_ubrk_first(UBreakIterator *bi);
int32_t __hs_ubrk_last(UBreakIterator *bi);
int32_t __hs_ubrk_next(UBreakIterator *bi);
int32_t __hs_ubrk_previous(UBreakIterator *bi);
int32_t __hs_ubrk_preceding(UBreakIterator *bi, int32_t offset);
int32_t __hs_ubrk_following(UBreakIterator *bi, int32_t offset);
int32_t __hs_ubrk_getRuleStatus(UBreakIterator *bi);
int32_t __hs_ubrk_getRuleStatusVec(UBreakIterator *bi, int32_t *fillInVec,
								   int32_t capacity, UErrorCode *status);
UBool __hs_ubrk_isBoundary(UBreakIterator *bi, int32_t offset);
int32_t __hs_ubrk_countAvailable(void);
const char *__hs_ubrk_getAvailable(int32_t index);

/* uchar.h */

UBlockCode __hs_ublock_getCode(UChar32 c);
UCharDirection __hs_u_charDirection(UChar32 c);
UBool __hs_u_isMirrored(UChar32 c);
UChar32 __hs_u_charMirror(UChar32 c);
uint8_t __hs_u_getCombiningClass(UChar32 c);
int32_t __hs_u_charDigitValue(UChar32 c);
int32_t __hs_u_charName(UChar32 code, UCharNameChoice nameChoice,
						char *buffer, int32_t bufferLength,
						UErrorCode *pErrorCode);
UChar32 __hs_u_charFromName(UCharNameChoice nameChoice,
							const char *name,
							UErrorCode *pErrorCode);
int32_t __hs_u_getIntPropertyValue(UChar32 c, UProperty which);
double __hs_u_getNumericValue(UChar32 c);

/* ucol.h */

UCollator *__hs_ucol_open(const char *loc, UErrorCode *status);
void __hs_ucol_close(UCollator *coll);
void __hs_ucol_setAttribute(UCollator *coll, UColAttribute attr,
							UColAttributeValue value, UErrorCode *status);
UColAttributeValue __hs_ucol_getAttribute(const UCollator *coll,
										  UColAttribute attr,
										  UErrorCode *status);
UCollationResult __hs_ucol_strcoll(const UCollator *coll,
								   const UChar *source, int32_t sourceLength,
								   const UChar *target, int32_t targetLength);
UCollationResult __hs_ucol_strcollIter(const UCollator *coll,
									   UCharIterator *sIter,
									   UCharIterator *tIter,
									   UErrorCode *status);
UCollator *__hs_ucol_safeClone(const UCollator *coll,
							   void *stackBuffer,
							   int32_t *pBufferSize,
							   UErrorCode *status);
int32_t __hs_ucol_getSortKey(const UCollator *coll,
							 const UChar *source, int32_t sourceLength,
							 uint8_t *result, int32_t resultLength);

/* ucnv.h */

int __get_max_bytes_for_string(UConverter *cnv, int src_length);
const char *__hs_u_errorName(UErrorCode code);
const char *__hs_ucnv_getName(const UConverter *converter, UErrorCode *err);
UConverter *__hs_ucnv_open(const char *converterName, UErrorCode *err);
void __hs_ucnv_close(UConverter *converter);
int32_t __hs_ucnv_toUChars(UConverter *cnv, UChar *dest, int32_t destCapacity,
						   const char *src, int32_t srcLength,
						   UErrorCode *pErrorCode);
int32_t __hs_ucnv_fromUChars(UConverter *cnv, char *dest, int32_t destCapacity,
							 const UChar *src, int32_t srcLength,
							 UErrorCode *pErrorCode);
int __hs_ucnv_compareNames(const char *name1, const char *name2);
const char *__hs_ucnv_getDefaultName(void);
void __hs_ucnv_setDefaultName(const char *name);
int32_t __hs_ucnv_countAvailable(void);
const char *__hs_ucnv_getAvailableName(int32_t n);
uint16_t __hs_ucnv_countAliases(const char *alias, UErrorCode *pErrorCode);
const char *__hs_ucnv_getAlias(const char *alias, uint16_t n,
							   UErrorCode *pErrorCode);
uint16_t __hs_ucnv_countStandards(void);
const char *__hs_ucnv_getStandard(uint16_t n, UErrorCode *pErrorCode);
UBool __hs_ucnv_usesFallback(const UConverter *cnv);
void __hs_ucnv_setFallback(UConverter *cnv, UBool usesFallback);
UBool __hs_ucnv_isAmbiguous(const UConverter *cnv);

/* uiter.h */

void __hs_uiter_setString(UCharIterator *iter, const UChar *s, int32_t length);
void __hs_uiter_setUTF8(UCharIterator *iter, const char *s, int32_t length);

/* unorm.h */

int32_t __hs_unorm_compare(const UChar *s1, int32_t length1,
						   const UChar *s2, int32_t length2,
						   uint32_t options,
						   UErrorCode *pErrorCode);
UNormalizationCheckResult __hs_unorm_quickCheck(const UChar *source,
												int32_t sourcelength,
												UNormalizationMode mode,
												UErrorCode *status);
UBool __hs_unorm_isNormalized(const UChar *src, int32_t srcLength,
							  UNormalizationMode mode,
							  UErrorCode *pErrorCode);
int32_t __hs_unorm_normalize(const UChar *source, int32_t sourceLength,
							 UNormalizationMode mode, int32_t options,
							 UChar *result, int32_t resultLength,
							 UErrorCode *status);

/* uregex.h */

URegularExpression *__hs_uregex_open(const UChar *pattern,
									 int32_t patternLength, uint32_t flags,
									 UParseError *pe, UErrorCode *status);
void __hs_uregex_setTimeLimit(URegularExpression *regexp,
							  int32_t limit, UErrorCode *status);
void __hs_uregex_setStackLimit(URegularExpression *regexp,
							   int32_t limit, UErrorCode *status);
void __hs_uregex_close(URegularExpression *regexp);
URegularExpression *__hs_uregex_clone(const URegularExpression *regexp,
									  UErrorCode *pErrorCode);
const UChar *__hs_uregex_pattern(const URegularExpression *regexp,
								 int32_t *patLength, UErrorCode *status);
int32_t __hs_uregex_flags(const URegularExpression *regexp,
						  UErrorCode *status);
void __hs_uregex_setText(URegularExpression *regexp, const UChar *text,
						 int32_t textLength, UErrorCode *status);
const UChar *__hs_uregex_getText(URegularExpression *regexp,
								 int32_t *textLength, UErrorCode *status);
UBool __hs_uregex_find(URegularExpression *regexp, int32_t startIndex,
					   UErrorCode *status);
UBool __hs_uregex_findNext(URegularExpression *regexp, UErrorCode *status);
int32_t __hs_uregex_start(URegularExpression *regexp, int32_t groupNum,
						  UErrorCode *status);
int32_t __hs_uregex_end(URegularExpression *regexp, int32_t groupNum,
						UErrorCode *status);
int32_t __hs_uregex_groupCount(URegularExpression *regexp, UErrorCode *status);
int32_t __hs_uregex_group(URegularExpression *regexp, int32_t groupNum,
						  UChar *dest, int32_t destCapacity,
						  UErrorCode *status);

/* ustring.h */

int32_t __hs_u_strFoldCase(UChar *dest, int32_t destCapacity,
						   const UChar *src, int32_t srcLength,
						   uint32_t options, UErrorCode *pErrorCode);
int32_t __hs_u_strToUpper(UChar *dest, int32_t destCapacity,
						  const UChar *src, int32_t srcLength,
						  const char *locale, UErrorCode *pErrorCode);
int32_t __hs_u_strToLower(UChar *dest, int32_t destCapacity,
						  const UChar *src, int32_t srcLength,
						  const char *locale, UErrorCode *pErrorCode);
int32_t __hs_u_strCompareIter(UCharIterator *iter1, UCharIterator *iter2);

/* uspoof.h */

USpoofChecker *__hs_uspoof_open(UErrorCode *status);
USpoofChecker *__hs_uspoof_openFromSerialized(const void *data, int32_t length,
											  int32_t *pActualLength,
											  UErrorCode *status);
USpoofChecker *__hs_uspoof_openFromSource(const char *confusables,
										  int32_t confusablesLen,
										  const char *confusablesWholeScript,
										  int32_t confusablesWholeScriptLen,
										  int32_t *errType,
										  UParseError *parseError,
										  UErrorCode *status);
void __hs_uspoof_setChecks(USpoofChecker *sc, int32_t checks,
						   UErrorCode *status);
int32_t __hs_uspoof_getChecks(const USpoofChecker *sc, UErrorCode *status);

// Yes, these really don't take UErrorCode *..
void __hs_uspoof_setRestrictionLevel(USpoofChecker *sc,
									 URestrictionLevel restrictionLevel);
URestrictionLevel __hs_uspoof_getRestrictionLevel(const USpoofChecker *sc);

void __hs_uspoof_setAllowedLocales(USpoofChecker *sc, const char *localesList,
								   UErrorCode *status);
const char *__hs_uspoof_getAllowedLocales(USpoofChecker *sc,
										  UErrorCode *status);

int32_t __hs_uspoof_check(USpoofChecker *sc, const UChar *id,
						  int32_t length, int32_t *position,
						  UErrorCode *status);
int32_t __hs_uspoof_areConfusable(USpoofChecker *sc,
								  const UChar *id1, int32_t length1,
								  const UChar *id2, int32_t length2,
								  UErrorCode *status);
int32_t __hs_uspoof_getSkeleton(USpoofChecker *sc, int32_t checks,
								const UChar *id, int32_t length,
								UChar *dest, int32_t destCapacity,
								UErrorCode *status);
int32_t __hs_uspoof_serialize(USpoofChecker *sc, void *data, int32_t capacity,
							  UErrorCode *status);
void __hs_uspoof_close(USpoofChecker *sc);
