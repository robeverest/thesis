
#ifndef MATRIX_MARKET_H
#define MATRIX_MARKET_H

#include "mmio.h"
#include <stdint.h>


typedef enum {
  MM_FIELD_REAL,
  MM_FIELD_COMPLEX,
  MM_FIELD_INTEGER,
  MM_FIELD_PATTERN
} MM_field_t;

typedef enum {
  MM_FORMAT_COORDINATE,
  MM_FORMAT_ARRAY
} MM_format_t;

typedef enum {
  MM_STRUCTURE_GENERAL,
  MM_STRUCTURE_SYMMETRIC,
  MM_STRUCTURE_HERMITIAN,
  MM_STRUCTURE_SKEW
} MM_structure_t;

typedef enum {
  MM_SUCCESS                    = 0,
  MM_ERROR_COULD_NOT_READ_FILE  = MM_COULD_NOT_READ_FILE,
  MM_ERROR_PREMATURE_EOF        = MM_PREMATURE_EOF,
  MM_ERROR_NOT_MTX              = MM_NOT_MTX,
  MM_ERROR_NO_HEADER            = MM_NO_HEADER,
  MM_ERROR_UNSUPPORTED_TYPE     = MM_UNSUPPORTED_TYPE,
  MM_ERROR_LINE_TOO_LONG        = MM_LINE_TOO_LONG,
  MM_ERROR_COULD_NOT_WRITE_FILE = MM_COULD_NOT_WRITE_FILE
} MM_status_t;


MM_status_t mm_read_general_real
(
    const char      *filename,
    int64_t         *n_rows,
    int64_t         *n_cols,
    int64_t         *n_nonzero,
    int32_t         **row_indices,  // row indices
    int32_t         **col_indices,  // column indices
    double          **values        // non-zero entries
);

#endif

