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
 * Programmer:  Quincey Koziol
 *              Monday, April 17, 2000
 *
 * Purpose:	The public header file for the log driver.
 */
#ifndef H5FDlog_H
#define H5FDlog_H

#define H5FD_LOG (H5FD_log_init())

/* Flags for H5Pset_fapl_log() */
/* Flags for tracking 'meta' operations (truncate) */
#define H5FD_LOG_TRUNCATE 0x00000001
#define H5FD_LOG_META_IO  (H5FD_LOG_TRUNCATE)
/* Flags for tracking where reads/writes/seeks occur */
#define H5FD_LOG_LOC_READ  0x00000002
#define H5FD_LOG_LOC_WRITE 0x00000004
#define H5FD_LOG_LOC_SEEK  0x00000008
#define H5FD_LOG_LOC_IO    (H5FD_LOG_LOC_READ | H5FD_LOG_LOC_WRITE | H5FD_LOG_LOC_SEEK)
/* Flags for tracking number of times each byte is read/written */
#define H5FD_LOG_FILE_READ  0x00000010
#define H5FD_LOG_FILE_WRITE 0x00000020
#define H5FD_LOG_FILE_IO    (H5FD_LOG_FILE_READ | H5FD_LOG_FILE_WRITE)
/* Flag for tracking "flavor" (type) of information stored at each byte */
#define H5FD_LOG_FLAVOR 0x00000040
/* Flags for tracking total number of reads/writes/seeks/truncates */
#define H5FD_LOG_NUM_READ     0x00000080
#define H5FD_LOG_NUM_WRITE    0x00000100
#define H5FD_LOG_NUM_SEEK     0x00000200
#define H5FD_LOG_NUM_TRUNCATE 0x00000400
#define H5FD_LOG_NUM_IO       (H5FD_LOG_NUM_READ | H5FD_LOG_NUM_WRITE | H5FD_LOG_NUM_SEEK | H5FD_LOG_NUM_TRUNCATE)
/* Flags for tracking time spent in open/stat/read/write/seek/truncate/close */
#define H5FD_LOG_TIME_OPEN     0x00000800
#define H5FD_LOG_TIME_STAT     0x00001000
#define H5FD_LOG_TIME_READ     0x00002000
#define H5FD_LOG_TIME_WRITE    0x00004000
#define H5FD_LOG_TIME_SEEK     0x00008000
#define H5FD_LOG_TIME_TRUNCATE 0x00010000
#define H5FD_LOG_TIME_CLOSE    0x00020000
#define H5FD_LOG_TIME_IO                                                                                     \
    (H5FD_LOG_TIME_OPEN | H5FD_LOG_TIME_STAT | H5FD_LOG_TIME_READ | H5FD_LOG_TIME_WRITE |                    \
     H5FD_LOG_TIME_SEEK | H5FD_LOG_TIME_TRUNCATE | H5FD_LOG_TIME_CLOSE)
/* Flags for tracking allocation/release of space in file */
#define H5FD_LOG_ALLOC 0x00040000
#define H5FD_LOG_FREE  0x00080000
#define H5FD_LOG_ALL                                                                                         \
    (H5FD_LOG_FREE | H5FD_LOG_ALLOC | H5FD_LOG_TIME_IO | H5FD_LOG_NUM_IO | H5FD_LOG_FLAVOR |                 \
     H5FD_LOG_FILE_IO | H5FD_LOG_LOC_IO | H5FD_LOG_META_IO)

#ifdef __cplusplus
extern "C" {
#endif

H5_DLL hid_t H5FD_log_init(void);

/**
 * \ingroup FAPL
 *
 * \brief Sets up the logging virtual file driver (#H5FD_LOG) for use
 *
 * \fapl_id
 * \param[in] logfile Name of the log file
 * \param[in] flags Flags specifying the types of logging activity
 * \param[in] buf_size The size of the logging buffers, in bytes (see description)
 * \returns \herr_t
 *
 * \details H5Pset_fapl_log() modifies the file access property list to use the
 *          logging driver, #H5FD_LOG. The logging virtual file driver (VFD) is
 *          a clone of the standard SEC2 (#H5FD_SEC2) driver with additional
 *          facilities for logging VFD metrics and activity to a file.
 *
 *          \p logfile is the name of the file in which the logging entries are
 *          to be recorded.
 *
 *          The actions to be logged are specified in the parameter \p flags
 *          using the pre-defined constants described in the following
 *          table. Multiple flags can be set through the use of a logical \c OR
 *          contained in parentheses. For example, logging read and write
 *          locations would be specified as
 *          \Code{(H5FD_LOG_LOC_READ|H5FD_LOG_LOC_WRITE)}.
 *
 * <table>
 * <caption>Table1: Logging Flags</caption>
 * <tr>
 * <td>
 * #H5FD_LOG_LOC_READ
 * </td>
 * <td rowspan="3">
 * Track the location and length of every read, write, or seek operation.
 * </td>
 * </tr>
 * <tr><td>#H5FD_LOG_LOC_WRITE</td></tr>
 * <tr><td>#H5FD_LOG_LOC_SEEK</td></tr>
 * <tr>
 * <td>
 * #H5FD_LOG_LOC_IO
 * </td>
 * <td>
 * Track all I/O locations and lengths. The logical equivalent of the following:
 * \Code{(#H5FD_LOG_LOC_READ | #H5FD_LOG_LOC_WRITE | #H5FD_LOG_LOC_SEEK)}
 * </td>
 * </tr>
 * <tr>
 * <td>
 * #H5FD_LOG_FILE_READ
 * </td>
 * <td rowspan="2">
 * Track the number of times each byte is read or written.
 * </td>
 * </tr>
 * <tr><td>#H5FD_LOG_FILE_WRITE</td></tr>
 * <tr>
 * <td>
 * #H5FD_LOG_FILE_IO
 * </td>
 * <td>
 * Track the number of times each byte is read and written. The logical
 * equivalent of the following:
 * \Code{(#H5FD_LOG_FILE_READ | #H5FD_LOG_FILE_WRITE)}
 * </td>
 * </tr>
 * <tr>
 * <td>
 * #H5FD_LOG_FLAVOR
 * </td>
 * <td>
 * Track the type, or flavor, of information stored at each byte.
 * </td>
 * </tr>
 * <tr>
 * <td>
 * #H5FD_LOG_NUM_READ
 * </td>
 * <td rowspan="4">
 * Track the total number of read, write, seek, or truncate operations that occur.
 * </td>
 * </tr>
 * <tr><td>#H5FD_LOG_NUM_WRITE</td></tr>
 * <tr><td>#H5FD_LOG_NUM_SEEK</td></tr>
 * <tr><td>#H5FD_LOG_NUM_TRUNCATE</td></tr>
 * <tr>
 * <td>
 * #H5FD_LOG_NUM_IO
 * </td>
 * <td>
 * Track the total number of all types of I/O operations. The logical equivalent
 * of the following:
 * \Code{(#H5FD_LOG_NUM_READ | #H5FD_LOG_NUM_WRITE | #H5FD_LOG_NUM_SEEK | #H5FD_LOG_NUM_TRUNCATE)}
 * </td>
 * </tr>
 * <tr>
 * <td>
 * #H5FD_LOG_TIME_OPEN
 * </td>
 * <td rowspan="6">
 * Track the time spent in open, stat, read, write, seek, or close operations.
 * </td>
 * </tr>
 * <tr><td>#H5FD_LOG_TIME_STAT</td></tr>
 * <tr><td>#H5FD_LOG_TIME_READ</td></tr>
 * <tr><td>#H5FD_LOG_TIME_WRITE</td></tr>
 * <tr><td>#H5FD_LOG_TIME_SEEK</td></tr>
 * <tr><td>#H5FD_LOG_TIME_CLOSE</td></tr>
 * <tr>
 * <td>
 * #H5FD_LOG_TIME_IO
 * </td>
 * <td>
 * Track the time spent in each of the above operations. The logical equivalent
 * of the following:
 * \Code{(#H5FD_LOG_TIME_OPEN | #H5FD_LOG_TIME_STAT | #H5FD_LOG_TIME_READ | #H5FD_LOG_TIME_WRITE |
 *        #H5FD_LOG_TIME_SEEK | #H5FD_LOG_TIME_CLOSE)}
 * </td>
 * </tr>
 * <tr>
 * <td>
 * #H5FD_LOG_ALLOC
 * </td>
 * <td>
 * Track the allocation of space in the file.
 * </td>
 * </tr>
 * <tr>
 * <td>
 * #H5FD_LOG_ALL
 * </td>
 * <td>
 * Track everything. The logical equivalent of the following:
 * \Code{(#H5FD_LOG_ALLOC | #H5FD_LOG_TIME_IO | #H5FD_LOG_NUM_IO | #H5FD_LOG_FLAVOR | #H5FD_LOG_FILE_IO |
 *        #H5FD_LOG_LOC_IO)}
 * </td>
 * </tr>
 * </table>
 * The logging driver can track the number of times each byte in the file is
 * read from or written to (using #H5FD_LOG_FILE_READ and #H5FD_LOG_FILE_WRITE)
 * and what kind of data is at that location (e.g., metadata, raw data; using
 * #H5FD_LOG_FLAVOR). This information is tracked in internal buffers of size
 * buf_size, which must be at least the maximum size in bytes of the file to be
 * logged while the log driver is in use.\n
 * One buffer of size buf_size will be created for each of #H5FD_LOG_FILE_READ,
 * #H5FD_LOG_FILE_WRITE and #H5FD_LOG_FLAVOR when those flags are set; these
 * buffers will not grow as the file increases in size.
 *
 * \par Output:
 * This section describes the logging driver (LOG VFD) output.\n
 * The table, immediately below, describes output of the various logging driver
 * flags and function calls. A list of valid flavor values, describing the type
 * of data stored, follows the table.
 * <table>
 * <caption>Table2: Logging Output</caption>
 * <tr>
 * <th>Flag</th><th>VFD Call</th><th>Output and Comments</th>
 * </th>
 * </tr>
 * <tr>
 * <td>#H5FD_LOG_LOC_READ</td>
 * <td>Read</td>
 * <td>
 * \Code{%10a-%10a (%10Zu bytes) (%s) Read}\n\n
 * Start position\n
 * End position\n
 * Number of bytes\n
 * Flavor of read\n\n
 * Adds \Code{(\%f s)} and seek time if #H5FD_LOG_TIME_SEEK is also set.
 * </td>
 * </tr>
 * <tr>
 * <td>#H5FD_LOG_LOC_READ</td>
 * <td>Read Error</td>
 * <td>
 * \Code{Error! Reading: %10a-%10a (%10Zu bytes)}\n\n
 * Same parameters as non-error entry.
 * </td>
 * </tr>
 * <tr>
 * <td>#H5FD_LOG_LOC_WRITE</td>
 * <td>Write</td>
 * <td>
 * \Code{%10a-%10a (%10Zu bytes) (%s) Written}\n\n
 * Start position\n
 * End position\n
 * Number of bytes\n
 * Flavor of write\n\n
 * Adds \Code{(\%f s)} and seek time if #H5FD_LOG_TIME_SEEK is also set.
 * </td>
 * </tr>
 * <tr>
 * <td>#H5FD_LOG_LOC_WRITE</td>
 * <td>Write Error</td>
 * <td>
 * \Code{Error! Writing: %10a-%10a (%10Zu bytes)}\n\n
 * Same parameters as non-error entry.
 * </td>
 * </tr>
 * <tr>
 * <td>#H5FD_LOG_LOC_SEEK</td>
 * <td>Read, Write</td>
 * <td>
 * \Code{Seek: From %10a-%10a}\n\n
 * Start position\n
 * End position\n\n
 * Adds \Code{(\%f s)} and seek time if #H5FD_LOG_TIME_SEEK is also set.
 * </td>
 * </tr>
 * <tr>
 * <td>#H5FD_LOG_FILE_READ</td>
 * <td>Close</td>
 * <td>
 * Begins with:\n
 * Dumping read I/O information\n\n
 * Then, for each range of identical values, there is this line:\n
 * \Code{Addr %10-%10 (%10lu bytes) read from %3d times}\n\n
 * Start address\n
 * End address\n
 * Number of bytes\n
 * Number of times read\n\n
 * Note: The data buffer is scanned and each range of identical values
 * gets one entry in the log file to save space and make it easier to read.
 * </td>
 * </tr>
 * <tr>
 * <td>#H5FD_LOG_FILE_WRITE</td>
 * <td>Close</td>
 * <td>
 * Begins with:\n
 * Dumping read I/O information\n\n
 * Then, for each range of identical values, there is this line:\n
 * \Code{Addr %10-%10 (%10lu bytes) written to %3d times}\n\n
 * Start address\n
 * End address\n
 * Number of bytes\n
 * Number of times written\n\n
 * Note: The data buffer is scanned and each range of identical values
 * gets one entry in the log file to save space and make it easier to read.
 * </td>
 * </tr>
 * <tr>
 * <td>#H5FD_LOG_FLAVOR</td>
 * <td>Close</td>
 * <td>
 * Begins with:\n
 * Dumping I/O flavor information\n\n
 * Then, for each range of identical values, there is this line:\n
 * \Code{Addr %10-%10 (%10lu bytes) flavor is %s}\n\n
 * Start address\n
 * End address\n
 * Number of bytes\n
 * Flavor\n\n
 * Note: The data buffer is scanned and each range of identical values
 * gets one entry in the log file to save space and make it easier to read.
 * </td>
 * </tr>
 * <tr>
 * <td>#H5FD_LOG_NUM_READ</td>
 * <td>Close</td>
 * <td>
 * Total number of read operations: \Code{%11u}
 * </td>
 * </tr>
 * <tr>
 * <td>#H5FD_LOG_NUM_WRITE</td>
 * <td>Close</td>
 * <td>
 * Total number of write operations: \Code{%11u}
 * </td>
 * </tr>
 * <tr>
 * <td>#H5FD_LOG_NUM_SEEK</td>
 * <td>Close</td>
 * <td>
 * Total number of seek operations: \Code{%11u}
 * </td>
 * </tr>
 * <tr>
 * <td>#H5FD_LOG_NUM_TRUNCATE</td>
 * <td>Close</td>
 * <td>
 * Total number of truncate operations: \Code{%11u}
 * </td>
 * </tr>
 * <tr>
 * <td>#H5FD_LOG_TIME_OPEN</td>
 * <td>Open</td>
 * <td>
 * Open took: \Code{(\%f s)}
 * </td>
 * </tr>
 * <tr>
 * <td>#H5FD_LOG_TIME_READ</td>
 * <td>Close, Read</td>
 * <td>
 * Total time in read operations: \Code{\%f s}\n\n
 * See also: #H5FD_LOG_LOC_READ
 * </td>
 * </tr>
 * </tr>
 * <tr>
 * <td>#H5FD_LOG_TIME_WRITE</td>
 * <td>Close, Write</td>
 * <td>
 * Total time in write operations: \Code{\%f s}\n\n
 * See also: #H5FD_LOG_LOC_WRITE
 * </td>
 * </tr>
 * <tr>
 * <td>#H5FD_LOG_TIME_SEEK</td>
 * <td>Close, Read, Write</td>
 * <td>
 * Total time in write operations: \Code{\%f s}\n\n
 * See also: #H5FD_LOG_LOC_SEEK or #H5FD_LOG_LOC_WRITE
 * </td>
 * </tr>
 * <tr>
 * <td>#H5FD_LOG_TIME_CLOSE</td>
 * <td>Close</td>
 * <td>
 * Close took: \Code{(\%f s)}
 * </td>
 * </tr>
 * <tr>
 * <td>#H5FD_LOG_TIME_STAT</td>
 * <td>Open</td>
 * <td>
 * Stat took: \Code{(\%f s)}
 * </td>
 * </tr>
 * <tr>
 * <td>#H5FD_LOG_ALLOC</td>
 * <td>Alloc</td>
 * <td>
 * \Code{%10-%10 (%10Hu bytes) (\%s) Allocated}\n\n
 * Start of address space\n
 * End of address space\n
 * Total size allocation\n
 * Flavor of allocation
 * </td>
 * </tr>
 * </table>
 *
 * \par Flavors:
 * The \Emph{flavor} describes the type of stored information. The following
 * table lists the flavors that appear in log output and briefly describes each.
 * These terms are provided here to aid in the construction of log message
 * parsers; a full description is beyond the scope of this document.
 * <table>
 * <caption>Table3: Flavors of logged data</caption>
 * <tr>
 * <th>Flavor</th><th>Description</th>
 * </th>
 * </tr>
 * <tr>
 * <td>#H5FD_MEM_NOLIST</td>
 * <td>Error value</td>
 * </tr>
 * <tr>
 * <td>#H5FD_MEM_DEFAULT</td>
 * <td>Value not yet set.\n
 *     May also be a datatype set in a larger allocation that will be
 *     suballocated by the library.</td>
 * </tr>
 * <tr>
 * <td>#H5FD_MEM_SUPER</td>
 * <td>Superblock data</td>
 * </tr>
 * <tr>
 * <td>#H5FD_MEM_BTREE</td>
 * <td>B-tree data</td>
 * </tr>
 * <tr>
 * <td>#H5FD_MEM_DRAW</td>
 * <td>Raw data (for example, contents of a dataset)</td>
 * </tr>
 * <tr>
 * <td>#H5FD_MEM_GHEAP</td>
 * <td>Global heap data</td>
 * </tr>
 * <tr>
 * <td>#H5FD_MEM_LHEAP</td>
 * <td>Local heap data</td>
 * </tr>
 * <tr>
 * <td>#H5FD_MEM_OHDR</td>
 * <td>Object header data</td>
 * </tr>
 * </table>
 *
 * \version 1.8.7 The flags parameter has been changed from \Code{unsigned int}
 *          to \Code{unsigned long long}.
 *          The implementation of the #H5FD_LOG_TIME_OPEN, #H5FD_LOG_TIME_READ,
 *          #H5FD_LOG_TIME_WRITE, and #H5FD_LOG_TIME_SEEK flags has been finished.
 *          New flags were added: #H5FD_LOG_NUM_TRUNCATE and #H5FD_LOG_TIME_STAT.
 * \version 1.6.0 The \c verbosity parameter has been removed.
 *          Two new parameters have been added: \p flags of type \Code{unsigned} and
 *          \p buf_size of type \Code{size_t}.
 * \since 1.4.0
 *
 */
H5_DLL herr_t H5Pset_fapl_log(hid_t fapl_id, const char *logfile, unsigned long long flags, size_t buf_size);

#ifdef __cplusplus
}
#endif

#endif
