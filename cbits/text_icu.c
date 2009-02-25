#include "unicode/ucnv.h"

#include <stdint.h>

int __get_max_bytes_for_string(int src_length, UConverter *cnv)
{
    return UCNV_GET_MAX_BYTES_FOR_STRING(src_length, ucnv_getMaxCharSize(cnv));
}

int __u_failure(UErrorCode err)
{
    return U_FAILURE(err);
}
