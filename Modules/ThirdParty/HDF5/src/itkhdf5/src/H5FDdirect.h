/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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
 * Programmer:  Raymond Lu
 *              Wednesday, 20 September 2006
 *
 * Purpose:	The public header file for the direct driver.
 */
#ifndef H5FDdirect_H
#define H5FDdirect_H

#ifdef H5_HAVE_DIRECT
#define H5FD_DIRECT (H5FD_direct_init())
#else
#define H5FD_DIRECT (H5I_INVALID_HID)
#endif /* H5_HAVE_DIRECT */

#ifdef H5_HAVE_DIRECT
#ifdef __cplusplus
extern "C" {
#endif

/* Default values for memory boundary, file block size, and maximal copy buffer size.
 * Application can set these values through the function H5Pset_fapl_direct. */
#define MBOUNDARY_DEF 4096
#define FBSIZE_DEF    4096
#define CBSIZE_DEF    16 * 1024 * 1024

H5_DLL hid_t H5FD_direct_init(void);

/**
 * \ingroup FAPL
 *
 * \brief Sets up use of the direct I/O driver
 *
 * \fapl_id
 * \param[in] alignment Required memory alignment boundary
 * \param[in] block_size File system block size
 * \param[in] cbuf_size Copy buffer size
 * \returns \herr_t
 *
 * \details H5Pset_fapl_direct() sets the file access property list, \p fapl_id,
 *          to use the direct I/O driver, #H5FD_DIRECT. With this driver, data
 *          is written to or read from the file synchronously without being
 *          cached by the system.
 *
 *          File systems usually require the data address in memory, the file
 *          address, and the size of the data to be aligned. The HDF5 library’s
 *          direct I/O driver is able to handle unaligned data, though that will
 *          consume some additional memory resources and may slow
 *          performance. To get better performance, use the system function \p
 *          posix_memalign to align the data buffer in memory and the HDF5
 *          function H5Pset_alignment() to align the data in the file. Be aware,
 *          however, that aligned data I/O may cause the HDF5 file to be bigger
 *          than the actual data size would otherwise require because the
 *          alignment may leave some holes in the file.
 *
 *          \p alignment specifies the required alignment boundary in memory.
 *
 *          \p block_size specifies the file system block size. A value of 0
 *          (zero) means to use HDF5 library’s default value of 4KB.
 *
 *          \p cbuf_size specifies the copy buffer size.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Pset_fapl_direct(hid_t fapl_id, size_t alignment, size_t block_size, size_t cbuf_size);

/**
 * \ingroup FAPL
 *
 * \brief Retrieves direct I/O driver settings
 *
 * \fapl_id
 * \param[out] boundary Required memory alignment boundary
 * \param[out] block_size File system block size
 * \param[out] cbuf_size Copy buffer size
 * \returns \herr_t
 *
 * \details H5Pget_fapl_direct() retrieves the required memory alignment (\p
 *          alignment), file system block size (\p block_size), and copy buffer
 *          size (\p cbuf_size) settings for the direct I/O driver, #H5FD_DIRECT,
 *          from the file access property list \p fapl_id.
 *
 *          See H5Pset_fapl_direct() for discussion of these values,
 *          requirements, and important considerations.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Pget_fapl_direct(hid_t fapl_id, size_t *boundary /*out*/, size_t *block_size /*out*/,
                                 size_t *cbuf_size /*out*/);

#ifdef __cplusplus
}
#endif

#endif /* H5_HAVE_DIRECT */

#endif
