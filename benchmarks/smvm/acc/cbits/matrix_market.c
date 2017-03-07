
#include "matrix_market.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>


/*
 * Read a matrix as a general real matrix. [Skew]-Symmetric and Hermitian
 * matrices will have the lower triangle elements duplicated. If this is a
 * pattern matrix the non-zeros array will be NULL.
 */
MM_status_t mm_read_general_real
(
    const char      *filename,
    int64_t         *n_rows,
    int64_t         *n_cols,
    int64_t         *n_nonzero,
    int32_t         **row_indices,  // row indices
    int32_t         **col_indices,  // column indices
    double          **values        // non-zero entries
)
{
    FILE      *fp;
    int       rows, cols, nnz, entries = 0;

    int32_t   *iA = NULL;
    int32_t   *jA = NULL;
    double    *vA = NULL;

    MM_status_t status;
    MM_typecode matcode;

    if ((fp = fopen(filename, "r")) == NULL) {
        return MM_COULD_NOT_READ_FILE;
    }

    if ((status = mm_read_banner(fp, &matcode)) != MM_SUCCESS) {
        fclose(fp);
        return status;
    }

    /*
     * Determine matrix field type
     */
    if (mm_is_complex(matcode) || mm_is_integer(matcode)) {
        fclose(fp);
        return MM_ERROR_UNSUPPORTED_TYPE;
    }

    /*
     * Read size information
     */
    if ((status = mm_read_mtx_crd_size(fp, &rows, &cols, &nnz)) != MM_SUCCESS) {
        fclose(fp);
        return status;
    }

    /*
     * Reserve memory for elements.
     *
     * We replicate the lower triangle elements to the upper triangle in case of
     * [skew-]symmetric & Hermitian matrices.
     *
     */
    if (mm_is_general(matcode)) {
        iA = (int32_t*) malloc(nnz * sizeof(int32_t));
        jA = (int32_t*) malloc(nnz * sizeof(int32_t));

        if (mm_is_real(matcode)) {
            vA = (double*) malloc(nnz * sizeof(double));
            if (!vA) {
                fprintf(stderr, "failed to allocate arrays (size=%d)\n", nnz);
                exit(1);
            }
        }
    } else {
        iA = (int32_t*) malloc(2 * nnz * sizeof(int32_t));
        jA = (int32_t*) malloc(2 * nnz * sizeof(int32_t));

        if (mm_is_real(matcode)) {
            vA = (double*) malloc(2 * nnz * sizeof(double));
            if (!vA) {
                fprintf(stderr, "failed to allocate arrays (size=%d)\n", nnz);
                exit(1);
            }
        }
    }

    if (!iA || !jA) {
      fprintf(stderr, "failed to allocate arrays (size=%d)\n", nnz);
      exit(1);
    }

    /*
     * Parse elements
     */
    if (mm_is_general(matcode)) {
        int32_t y, x;

        for (int i = 0; i < nnz; ++i) {
            if (mm_is_real(matcode)) {
                fscanf(fp, "%d %d %le\n", &y, &x, &vA[i]);
            } else {
                fscanf(fp, "%d %d\n", &y, &x);
            }

            iA[i] = y-1;
            jA[i] = x-1;
        }
        entries = nnz;
    }
    else {
        int32_t y, x;
        double v;

        for (int i = 0; i < nnz; ++i) {
            if (mm_is_real(matcode)) {
                fscanf(fp, "%d %d %le\n", &y, &x, &v);
            } else {
                fscanf(fp, "%d %d\n", &y, &x);
            }

            y--;
            x--;

            if (x == y) {
                iA[entries] = y;
                jA[entries] = x;

                if (mm_is_real(matcode)) {
                    vA[entries] = v;
                }

                entries += 1;

            } else {
                iA[entries]   = y;
                jA[entries]   = x;
                iA[entries+1] = x;
                jA[entries+1] = y;

                if (mm_is_real(matcode)) {
                    vA[entries]   = v;
                    vA[entries+1] = v;
                }

                entries += 2;
            }
        }
    }

    *n_rows       = rows;
    *n_cols       = cols;
    *n_nonzero    = entries;
    *row_indices  = iA;
    *col_indices  = jA;
    *values       = vA;

    fclose(fp);
    return MM_SUCCESS;
}

