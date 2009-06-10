#ifdef WIN32
#define U_HAVE_INTTYPES_H 1
#endif

#include "unicode/ucnv.h"

#include <stdint.h>

int __get_max_bytes_for_string(UConverter *cnv, int src_length)
{
    return UCNV_GET_MAX_BYTES_FOR_STRING(src_length, ucnv_getMaxCharSize(cnv));
}
