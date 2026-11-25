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
 * Purpose: This file contains declarations which define macros for the
 *          H5PL package.  Including this header means that the source file
 *          is part of the H5PL package.
 */

#ifndef H5PLmodule_H
#define H5PLmodule_H

/* Define the proper control macros for the generic FUNC_ENTER/LEAVE and error
 *      reporting macros.
 */
#define H5PL_MODULE
#define H5_MY_PKG     H5PL
#define H5_MY_PKG_ERR H5E_PLUGIN

/** \page H5PL_UG HDF5 Plugins
 *
 * \section sec_filter_plugins HDF5 Filter Plugins
 *
 * \subsection subsec_filter_plugins_intro Introduction
 * HDF5 supports compression of data using a stackable pipeline of filters which can be
 * implemented for reading and writing datasets, both at runtime and post‐process.
 * These filters are supported as dynamically loadable plugins, and users can even
 * implement custom filters of their own design.
 *
 * \subsection subsec_filter_plugins_model Programming Model for Applications
 * This section describes the programming model for an application that uses a third-party HDF5 filter
 * plugin to write or read data. For simplicity of presentation, it is assumed that the HDF5 filter plugin is
 * available on the system in a default location. The HDF5 filter plugin is discussed in detail in the
 * \ref subsec_filter_plugins_prog section.
 *
 * \subsubsection subsubsec_filter_plugins_model_apply Applying a Third-party Filter When Creating and Writing
 * a Dataset A third-party filter can be added to the HDF5 filter pipeline by using the H5Pset_filter
 * function, as a user would do in the past. The identification number and the filter parameters should be
 * available to the application. For example, if the application intends to apply the HDF5 bzip2 compression
 * filter that was registered with The HDF Group and has an identification number 307
 * (<a href="https://github.com/HDFGroup/hdf5_plugins/blob/master/docs/RegisteredFilterPlugins.md">Registered
 * Filters</a>) then the application would follow the steps as outlined below: \code dcpl = H5Pcreate
 * (H5P_DATASET_CREATE); status = H5Pset_filter (dcpl, (H5Z_filter_t)307, H5Z_FLAG_MANDATORY, (size_t)6,
 * cd_values); dset = H5Dcreate (file, DATASET, H5T_STD_I32LE, space, H5P_DEFAULT, dcpl, status = H5Dwrite
 * (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata[0]); \endcode
 *
 * \subsubsection subsubsec_filter_plugins_model_read Reading Data with an Applied Third-party Filter
 * An application does not need to do anything special to read the data with a third-party filter applied. For
 * example, if one wants to read data written in the previous example, the following regular steps should be
 * taken: \code file = H5Fopen (FILE, H5F_ACC_RDONLY, H5P_DEFAULT); dset = H5Dopen (file, DATASET,
 * H5P_DEFAULT); H5Dread (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata[0]); \endcode
 *
 * The command-line utility h5dump, for example, will read and display the data as shown:
 * \code
 *   HDF5 "h5ex_d_bzip2.h5" {
 *   GROUP "/" {
 *      DATASET "DS1" {
 *         DATATYPE  H5T_STD_I32LE
 *         DATASPACE  SIMPLE { ( 32, 64 ) / ( 32, 64 ) }
 *         STORAGE_LAYOUT {
 *            CHUNKED ( 4, 8 )
 *            SIZE 6410 (1.278:1 COMPRESSION)
 *         }
 *         FILTERS {
 *            USER_DEFINED_FILTER {
 *               FILTER_ID 307
 *               COMMENT HDF5 bzip2 filter; see
 *   https://github.com/HDFGroup/hdf5_plugins/blob/master/docs/RegisteredFilterPlugins.md
 *               PARAMS { 2 }
 *            }
 *         }
 *         FILLVALUE {
 *            FILL_TIME H5D_FILL_TIME_IFSET
 *            VALUE  H5D_FILL_VALUE_DEFAULT
 *         }
 *         ALLOCATION_TIME {
 *            H5D_ALLOC_TIME_INCR
 *         }
 *         DATA {
 *            ...
 *         }
 *      }
 *   }
 *   }
 * \endcode
 *
 * If the filter can not be loaded then h5dump will show the following:
 * \code
 *            ...
 *         }
 *         DATA {h5dump error: unable to print data
 *         }
 *            ...
 * \endcode
 *
 * \subsubsection subsubsec_filter_plugins_model_custom A Word of Caution When Using Custom Filters
 * Data goes through the HDF5 filter pipeline only when it is written to the file or read into application
 * memory space from the file. For example, the I/O operation is triggered with a call to #H5Fflush, or when
 * a data item (HDF5 metadata or a raw data chunk) is evicted from the cache or brought into the cache.
 * Please notice that #H5Dread/#H5Dwrite calls on the chunked datasets do not necessarily trigger I/O since
 * the HDF5 Library uses a separate chunk cache.
 *
 * A data item may remain in the cache until the HDF5 Library is closed. If the HDF5 plugin that has to be
 * applied to the data item becomes unavailable before the file and all objects in the file are closed, an
 * error will occur. The following example demonstrates the issue. Please notice the position of the
 * #H5Zunregister call:
 *
 * \code
 *  // Create a new group using compression.
 *  gcpl = H5Pcreate (H5P_GROUP_CREATE);
 *  status = H5Pset_filter(gcpl,H5Z_FILTER_BZIP2,H5Z_FLAG_MANDATORY,(size_t)1, cd_values);
 *  group = H5Gcreate (file, GNAME, H5P_DEFAULT, gcpl, H5P_DEFAULT);
 *  for (i=0; i < NGROUPS; i++) {
 *      sprintf(name, "group_%d", i);
 *      tmp_id = H5Gcreate (group, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
 *      status = H5Gclose(tmp_id);
 *   }
 *
 *  status = H5Pclose (gcpl);
 *  status = H5Gclose (group);
 *
 *  // Unregister the filter. Call to H5Fclose will fail because the library tries
 *  // to apply the filter that is not available anymore. This has a cascade effect
 *  // on H5Fclose.
 *  H5Zunregister(H5Z_FILTER_BZIP2);
 *  status = H5Fclose (file);
 * \endcode
 *
 * Here is an error stack produced by the program:
 * \code
 * HDF5-DIAG: Error detected in HDF5 (xx.yy.zz) thread 0:
 *   #000: H5F.c line **** in H5Fclose(): decrementing file ID failed
 *     major: Object atom
 *     minor: Unable to close file
 *   #001: H5I.c line **** in H5I_dec_app_ref(): can't decrement ID ref count
 *     major: Object atom
 *     minor: Unable to decrement reference count
 *   #002: H5F.c line **** in H5F_close(): can't close file
 *     major: File accessibility
 *     minor: Unable to close file
 *  ...
 *   #026: H5Z.c line **** in H5Z_find(): required filter is not registered
 *     major: Data filters
 *     minor: Object not found
 * \endcode
 *
 * To avoid the problem make sure to close all objects to which the filter is applied and flush them using
 * the H5Fflush call before unregistering the filter.
 *
 * \subsection subsec_filter_plugins_prog Programming Model for HDF5 Filter Plugins
 * This section describes how to create an HDF5 filter, an HDF5 filter plugin, and how to install the HDF5
 * plugin on the system.
 *
 * \subsubsection subsubsec_filter_plugins_prog_write Writing a Filter Function
 * The HDF5 filter function for the dynamically loaded filter feature should be written as any custom filter
 * described in <a href="https://\DOXURL/_f_i_l_t_e_r.html">Custom Filters</a>. See the
 * “Example” section, section 5, of that document to get an idea of the simple filter function, and see the
 * example of the more sophisticated HDF5 bzip2 filter function in the “Building an HDF5 bzip2 Plugin Example”
 * section. The HDF5 bzip2 filter function is also available for download from <a
 * href="https://github.com/HDFGroup/hdf5_plugins">Filter Plugin Repository</a>.
 *
 * The user has to remember a few things when writing an HDF5 filter function.
 * <ul><li>1. An HDF5 filter is bidirectional.
 * The filter handles both input and output to the file; a flag is passed to the filter to indicate the
 * direction.</li>
 * <li>2. An HDF5 filter operates on a buffer.
 * The filter reads data from a buffer, performs some sort of transformation on the data, places
 * the result in the same or new buffer, and returns the buffer pointer and size to the caller.</li>
 * <li>3. An HDF5 filter should return zero in the case of failure.</li></ul>
 *
 * The signature of the HDF5 filter function and the accompanying filter structure (see the section below)
 * are described in the HDF5 Reference Manual #H5Z_filter_t.
 *
 * \subsubsection subsubsec_filter_plugins_prog_reg Registering a Filter with The HDF Group
 * If you are writing a filter that will be used by others, it would be a good idea to request a filter
 * identification number and register it with The HDF Group. Please follow the procedure described at
 * <a href="https://github.com/HDFGroup/hdf5_plugins/blob/master/docs/RegisteredFilterPlugins.md">Registered
 * Filters</a>.
 *
 * The HDF Group anticipates that developers of HDF5 filter plugins will not only register new filters, but
 * will also provide links to the source code and/or binaries for the corresponding HDF5 filter plugins.
 *
 * It is very important for the users of the filter that developers provide filter information in the “name”
 * field of the filter structure, for example:
 * \code
 *   const H5Z_class2_t H5Z_BZIP2[1] = {{
 *     H5Z_CLASS_T_VERS,                    // H5Z_class_t version
 *     (H5Z_filter_t)H5Z_FILTER_BZIP2,      // Filter id number
 *     1,                                   // encoder_present flag (set to true)
 *     1,                                   // decoder_present flag (set to true)
 *     "HDF5 bzip2 filter; see
 *     https://github.com/HDFGroup/hdf5_plugins/blob/master/docs/RegisteredFilterPlugins.md",
 *                                          // Filter name for debugging
 *     NULL,                                // The "can apply" callback
 *     NULL,                                // The "set local" callback
 *     (H5Z_func_t)H5Z_filter_bzip2,        // The actual filter function
 *   }};
 * \endcode
 *
 * The HDF5 Library and command-line tools have access to the “name” field. An application can
 * use the H5Pget_filter<*> functions to retrieve information about the filters.
 *
 * Using the example of the structure above, the h5dump tool will print the string “HDF5 bzip2
 * filter found at …” pointing users to the applied filter (see the example in the \ref
 * subsubsec_filter_plugins_model_read section) thus solving the problem of the filter’s origin.
 *
 * \subsubsection subsubsec_filter_plugins_prog_create Creating an HDF5 Filter Plugin
 * The HDF5 filter plugin source should include:
 * <ul><li>1. The H5PLextern.h header file from the HDF5 distribution.</li>
 * <li>2. The definition of the filter structure (see the example shown in the section above).</li>
 * <li>3. The filter function (for example, H5Z_filter_bzip2).</li>
 * <li>4. The two functions necessary for the HDF5 Library to find the correct type of the plugin library
 * while loading it at runtime and to get information about the filter function:
 * <table><tr><td><code>H5PL_type_t H5PLget_plugin_type(void);</code></td></tr>
 * <tr><td><code>const void* H5PLget_plugin_info(void);</code></td></tr></table>
 * Here is an example of the functions above for the HDF5 bzip2 filter:
 * <table><tr><td><code>H5PL_type_t H5PLget_plugin_type(void) {return H5PL_TYPE_FILTER;}</code></td></tr>
 * <tr><td><code>const void* H5PLget_plugin_info(void) {return H5Z_BZIP2;}</code></td></tr></table>
 * </li>
 * <li>5. Other functions such as the source of the compression library may also be included.</li>
 * </ul>
 * Build the HDF5 filter plugin as a shared library. The following steps should be taken:
 * <ul><li>1. When compiling, point to the HDF5 header files.</li>
 * <li>2. Use the appropriate linking flags.</li>
 * <li>3. Link with any required external libraries. </li>
 * <li>4. For example, if libbz2.so is installed on a Linux system, the HDF5 bzip2 plugin library
 * libH5Zbzip2.so may be linked with libbz2.so instead of including bzip2 source into the
 * plugin library.
 * The complete example of the HDF5 bzip2 plugin library is provided at
 * <a href="https://github.com/HDFGroup/hdf5_plugins/tree/master/BZIP2">BZIP2 Filter Plugin</a>
 * and can be adopted for other plugins.</li></ul>
 *
 * \subsubsection subsubsec_filter_plugins_prog_install Installing an HDF5 Filter Plugin
 * The default directory for an HDF5 filter plugin library is defined on UNIX-like systems as
 * \code
 *   “/usr/local/hdf5/lib/plugin”
 * \endcode
 * and on Windows systems as
 * \code
 *   "%ALLUSERSPROFILE%/hdf5/lib/plugin".
 * \endcode
 *
 * The default path can be overwritten by a user with the #HDF5_PLUGIN_PATH environment variable.
 * Several directories can be specified for the search path using “:” as a path separator for UNIX-like
 * systems and “;” for Windows.
 *
 * Readers are encouraged to try the example in the “Building an HDF5 bzip2 Plugin Example” section.
 *
 * \subsection subsec_filter_plugins_design Design
 * Dynamic loading of the HDF5 filter plugin (or filter library) is triggered only by two events: when an
 * application calls the #H5Pset_filter function to set the filter for the first time, or when the data to
 * which the filter is applied is read for the first time.
 *
 * \subsection subsec_filter_plugins_build Building an HDF5 bzip2 Plugin Example
 * The HDF Group provides an repository of the HDF5 filter plugin that can be checked out from
 * <a href="https://github.com/HDFGroup/hdf5_plugins/tree/master/BZIP2">BZIP2 Filter Plugin</a>.
 *
 * It contains the source code for the bzip2
 * plugin library and an example that uses the plugin. It requires the HDF5 Library with the dynamically
 * loaded feature and the bzip2 library being available on the system.
 * The plugin and the example can be built using configure or CMake commands. For instructions on how
 * to build with CMake, see the README.txt file in the source code distribution. The bzip2 library that can
 * be built with CMake is available from:
 * \code
 *   GIT_URL: "https://github.com/libarchive/bzip2.git"
 *   GIT_BRANCH: "master"
 * \endcode
 *
 * See the documentation at
 * <a href="https://github.com/HDFGroup/hdf5_plugins/tree/master/docs">hdf5_plugins/docs</a> folder. In
 * particular:
 * <a
 * href="https://github.com/HDFGroup/hdf5_plugins/blob/master/docs/INSTALL_With_CMake.txt">INSTALL_With_CMake</a>
 * <a
 * href="https://github.com/HDFGroup/hdf5_plugins/blob/master/docs/USING_HDF5_AND_CMake.txt">USING_HDF5_AND_CMake</a>
 */

/**
 * \defgroup H5PL Dynamically-loaded Plugins (H5PL)
 *
 * Use the functions in this module to manage the loading behavior of HDF5
 * plugins.
 *
 * \attention The loading behavior of HDF5 plugins can be controlled via the
 *            functions described below and certain environment variables, such
 *            as \c HDF5_PLUGIN_PRELOAD  and \c HDF5_PLUGIN_PATH.
 *
 */

#endif /* H5PLmodule_H */
