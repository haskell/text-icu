#ifdef WIN32
#define U_HAVE_INTTYPES_H 1
#endif

#include "unicode/utypes.h"
#include "unicode/ucol.h"
#include "unicode/ucnv.h"
#include "unicode/unorm.h"

#include <stdint.h>

/* ucol.h */

UCollator* __hs_ucol_open(const char *loc, UErrorCode *status);
void __hs_ucol_close(UCollator *coll);
UCollationResult __hs_ucol_strcoll(const UCollator *coll,
				   const UChar *source, int32_t sourceLength,
				   const UChar *target, int32_t targetLength);
UCollator* __hs_ucol_safeClone(const UCollator *coll,
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
UConverter* __hs_ucnv_open(const char *converterName, UErrorCode *err);
void __hs_ucnv_close(UConverter * converter);
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
const char* __hs_ucnv_getAvailableName(int32_t n);
uint16_t __hs_ucnv_countAliases(const char *alias, UErrorCode *pErrorCode);
const char *__hs_ucnv_getAlias(const char *alias, uint16_t n,
			       UErrorCode *pErrorCode);
uint16_t __hs_ucnv_countStandards(void);
const char *__hs_ucnv_getStandard(uint16_t n, UErrorCode *pErrorCode);
UBool __hs_ucnv_usesFallback(const UConverter *cnv);
void __hs_ucnv_setFallback(UConverter *cnv, UBool usesFallback);
UBool __hs_ucnv_isAmbiguous(const UConverter *cnv);

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
