/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Error handling for the HDF5 Subfiling feature
 */

#ifndef H5SUBFILING_ERR_H
#define H5SUBFILING_ERR_H

#include <errno.h>

#include "H5Epublic.h"

extern hid_t H5subfiling_err_stack_g;
extern hid_t H5subfiling_err_class_g;

#define H5SUBFILING_ERR_CLS_NAME "HDF5 Subfiling"
#define H5SUBFILING_ERR_LIB_NAME "HDF5 Subfiling"
#define H5SUBFILING_ERR_VER      "1.0.0"

/* Error macros */

#ifdef H5_NO_DEPRECATED_SYMBOLS

/*
 * Macro to push the current function to the current error stack
 * and then goto the "done" label, which should appear inside the
 * function. (v2 errors only)
 */
#define H5_SUBFILING_GOTO_ERROR(err_major, err_minor, ret_val, ...)                                          \
    do {                                                                                                     \
        H5E_auto2_t err_func;                                                                                \
                                                                                                             \
        /* Check whether automatic error reporting has been disabled */                                      \
        (void)H5Eget_auto2(H5E_DEFAULT, &err_func, NULL);                                                    \
        if (err_func) {                                                                                      \
            if (H5subfiling_err_stack_g >= 0 && H5subfiling_err_class_g >= 0) {                              \
                H5Epush2(H5subfiling_err_stack_g, __FILE__, __func__, __LINE__, H5subfiling_err_class_g,     \
                         err_major, err_minor, __VA_ARGS__);                                                 \
            }                                                                                                \
            else {                                                                                           \
                fprintf(stderr, __VA_ARGS__);                                                                \
                fprintf(stderr, "\n");                                                                       \
            }                                                                                                \
        }                                                                                                    \
                                                                                                             \
        ret_value = ret_val;                                                                                 \
        goto done;                                                                                           \
    } while (0)

/*
 * Macro to push the current function to the current error stack
 * without calling goto. This is used for handling the case where
 * an error occurs during cleanup past the "done" label inside a
 * function so that an infinite loop does not occur where goto
 * continually branches back to the label. (v2 errors only)
 */
#define H5_SUBFILING_DONE_ERROR(err_major, err_minor, ret_val, ...)                                          \
    do {                                                                                                     \
        H5E_auto2_t err_func;                                                                                \
                                                                                                             \
        /* Check whether automatic error reporting has been disabled */                                      \
        (void)H5Eget_auto2(H5E_DEFAULT, &err_func, NULL);                                                    \
        if (err_func) {                                                                                      \
            if (H5subfiling_err_stack_g >= 0 && H5subfiling_err_class_g >= 0)                                \
                H5Epush2(H5subfiling_err_stack_g, __FILE__, __func__, __LINE__, H5subfiling_err_class_g,     \
                         err_major, err_minor, __VA_ARGS__);                                                 \
            else {                                                                                           \
                fprintf(stderr, __VA_ARGS__);                                                                \
                fprintf(stderr, "\n");                                                                       \
            }                                                                                                \
        }                                                                                                    \
                                                                                                             \
        ret_value = ret_val;                                                                                 \
    } while (0)

/*
 * Macro to print out the current error stack and then clear it
 * for future use. (v2 errors only)
 */
#define PRINT_ERROR_STACK                                                                                    \
    do {                                                                                                     \
        H5E_auto2_t err_func;                                                                                \
                                                                                                             \
        /* Check whether automatic error reporting has been disabled */                                      \
        (void)H5Eget_auto2(H5E_DEFAULT, &err_func, NULL);                                                    \
        if (err_func) {                                                                                      \
            if ((H5subfiling_err_stack_g >= 0) && (H5Eget_num(H5subfiling_err_stack_g) > 0)) {               \
                H5Eprint2(H5subfiling_err_stack_g, NULL);                                                    \
                H5Eclear2(H5subfiling_err_stack_g);                                                          \
            }                                                                                                \
        }                                                                                                    \
    } while (0)

#else /* H5_NO_DEPRECATED_SYMBOLS */

/*
 * Macro to push the current function to the current error stack
 * and then goto the "done" label, which should appear inside the
 * function. (compatible with v1 and v2 errors)
 */
#define H5_SUBFILING_GOTO_ERROR(err_major, err_minor, ret_val, ...)                                          \
    do {                                                                                                     \
        unsigned is_v2_err;                                                                                  \
        union {                                                                                              \
            H5E_auto1_t err_func_v1;                                                                         \
            H5E_auto2_t err_func_v2;                                                                         \
        } err_func;                                                                                          \
                                                                                                             \
        /* Determine version of error */                                                                     \
        (void)H5Eauto_is_v2(H5E_DEFAULT, &is_v2_err);                                                        \
                                                                                                             \
        if (is_v2_err)                                                                                       \
            (void)H5Eget_auto2(H5E_DEFAULT, &err_func.err_func_v2, NULL);                                    \
        else                                                                                                 \
            (void)H5Eget_auto1(&err_func.err_func_v1, NULL);                                                 \
                                                                                                             \
        /* Check whether automatic error reporting has been disabled */                                      \
        if ((is_v2_err && err_func.err_func_v2) || (!is_v2_err && err_func.err_func_v1)) {                   \
            if (H5subfiling_err_stack_g >= 0 && H5subfiling_err_class_g >= 0) {                              \
                H5Epush2(H5subfiling_err_stack_g, __FILE__, __func__, __LINE__, H5subfiling_err_class_g,     \
                         err_major, err_minor, __VA_ARGS__);                                                 \
            }                                                                                                \
            else {                                                                                           \
                fprintf(stderr, __VA_ARGS__);                                                                \
                fprintf(stderr, "\n");                                                                       \
            }                                                                                                \
        }                                                                                                    \
                                                                                                             \
        ret_value = ret_val;                                                                                 \
        goto done;                                                                                           \
    } while (0)

/*
 * Macro to push the current function to the current error stack
 * without calling goto. This is used for handling the case where
 * an error occurs during cleanup past the "done" label inside a
 * function so that an infinite loop does not occur where goto
 * continually branches back to the label. (compatible with v1
 * and v2 errors)
 */
#define H5_SUBFILING_DONE_ERROR(err_major, err_minor, ret_val, ...)                                          \
    do {                                                                                                     \
        unsigned is_v2_err;                                                                                  \
        union {                                                                                              \
            H5E_auto1_t err_func_v1;                                                                         \
            H5E_auto2_t err_func_v2;                                                                         \
        } err_func;                                                                                          \
                                                                                                             \
        /* Determine version of error */                                                                     \
        (void)H5Eauto_is_v2(H5E_DEFAULT, &is_v2_err);                                                        \
                                                                                                             \
        if (is_v2_err)                                                                                       \
            (void)H5Eget_auto2(H5E_DEFAULT, &err_func.err_func_v2, NULL);                                    \
        else                                                                                                 \
            (void)H5Eget_auto1(&err_func.err_func_v1, NULL);                                                 \
                                                                                                             \
        /* Check whether automatic error reporting has been disabled */                                      \
        if ((is_v2_err && err_func.err_func_v2) || (!is_v2_err && err_func.err_func_v1)) {                   \
            if (H5subfiling_err_stack_g >= 0 && H5subfiling_err_class_g >= 0) {                              \
                H5Epush2(H5subfiling_err_stack_g, __FILE__, __func__, __LINE__, H5subfiling_err_class_g,     \
                         err_major, err_minor, __VA_ARGS__);                                                 \
            }                                                                                                \
            else {                                                                                           \
                fprintf(stderr, __VA_ARGS__);                                                                \
                fprintf(stderr, "\n");                                                                       \
            }                                                                                                \
        }                                                                                                    \
                                                                                                             \
        ret_value = ret_val;                                                                                 \
    } while (0)

/*
 * Macro to print out the current error stack and then clear it
 * for future use. (compatible with v1 and v2 errors)
 */
#define PRINT_ERROR_STACK                                                                                    \
    do {                                                                                                     \
        unsigned is_v2_err;                                                                                  \
        union {                                                                                              \
            H5E_auto1_t err_func_v1;                                                                         \
            H5E_auto2_t err_func_v2;                                                                         \
        } err_func;                                                                                          \
                                                                                                             \
        /* Determine version of error */                                                                     \
        (void)H5Eauto_is_v2(H5E_DEFAULT, &is_v2_err);                                                        \
                                                                                                             \
        if (is_v2_err)                                                                                       \
            (void)H5Eget_auto2(H5E_DEFAULT, &err_func.err_func_v2, NULL);                                    \
        else                                                                                                 \
            (void)H5Eget_auto1(&err_func.err_func_v1, NULL);                                                 \
                                                                                                             \
        /* Check whether automatic error reporting has been disabled */                                      \
        if ((is_v2_err && err_func.err_func_v2) || (!is_v2_err && err_func.err_func_v1)) {                   \
            if ((H5subfiling_err_stack_g >= 0) && (H5Eget_num(H5subfiling_err_stack_g) > 0)) {               \
                H5Eprint2(H5subfiling_err_stack_g, NULL);                                                    \
                H5Eclear2(H5subfiling_err_stack_g);                                                          \
            }                                                                                                \
        }                                                                                                    \
    } while (0)

#endif /* H5_NO_DEPRECATED_SYMBOLS */

#define H5_SUBFILING_SYS_GOTO_ERROR(err_major, err_minor, ret_val, str)                                      \
    do {                                                                                                     \
        int myerrno = errno;                                                                                 \
        H5_SUBFILING_GOTO_ERROR(err_major, err_minor, ret_val, "%s, errno = %d, error message = '%s'", str,  \
                                myerrno, strerror(myerrno));                                                 \
    } while (0)

/* MPI error handling macros. */

extern char H5subfiling_mpi_error_str[MPI_MAX_ERROR_STRING];
extern int  H5subfiling_mpi_error_str_len;

#define H5_SUBFILING_MPI_DONE_ERROR(retcode, str, mpierr)                                                    \
    do {                                                                                                     \
        MPI_Error_string(mpierr, H5subfiling_mpi_error_str, &H5subfiling_mpi_error_str_len);                 \
        H5_SUBFILING_DONE_ERROR(H5E_INTERNAL, H5E_MPI, retcode, "%s: MPI error string is '%s'", str,         \
                                H5subfiling_mpi_error_str);                                                  \
    } while (0)
#define H5_SUBFILING_MPI_GOTO_ERROR(retcode, str, mpierr)                                                    \
    do {                                                                                                     \
        MPI_Error_string(mpierr, H5subfiling_mpi_error_str, &H5subfiling_mpi_error_str_len);                 \
        H5_SUBFILING_GOTO_ERROR(H5E_INTERNAL, H5E_MPI, retcode, "%s: MPI error string is '%s'", str,         \
                                H5subfiling_mpi_error_str);                                                  \
    } while (0)

/*
 * Macro to simply jump to the "done" label inside the function,
 * setting ret_value to the given value. This is often used for
 * short circuiting in functions when certain conditions arise.
 */
#define H5_SUBFILING_GOTO_DONE(ret_val)                                                                      \
    do {                                                                                                     \
        ret_value = ret_val;                                                                                 \
        goto done;                                                                                           \
    } while (0)

/*
 * Macro to return from a top-level API function, printing
 * out the error stack on the way out.
 * It should be ensured that this macro is only called once
 * per HDF5 operation. If it is called multiple times per
 * operation (e.g. due to calling top-level API functions
 * internally), the error stack will be inconsistent/incoherent.
 */
#define H5_SUBFILING_FUNC_LEAVE_API                                                                          \
    do {                                                                                                     \
        PRINT_ERROR_STACK;                                                                                   \
        return ret_value;                                                                                    \
    } while (0)

/*
 * Macro to return from internal functions.
 */
#define H5_SUBFILING_FUNC_LEAVE                                                                              \
    do {                                                                                                     \
        return ret_value;                                                                                    \
    } while (0)

#endif /* H5SUBFILING_ERR_H */
