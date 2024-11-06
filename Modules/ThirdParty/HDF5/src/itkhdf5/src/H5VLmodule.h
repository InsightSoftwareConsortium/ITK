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
 * Purpose:	This file contains declarations which define macros for the
 *          H5VL package.  Including this header means that the source file
 *          is part of the H5VL package.
 */

#ifndef H5VLmodule_H
#define H5VLmodule_H

/* Define the proper control macros for the generic FUNC_ENTER/LEAVE and error
 *      reporting macros.
 */
#define H5VL_MODULE
#define H5_MY_PKG      H5VL
#define H5_MY_PKG_ERR  H5E_VOL
#define H5_MY_PKG_INIT YES

/** \page H5VL_UG The HDF5 Virtual Object Layer (VOL)
 *
 * \section sec_vol The HDF5 Virtual Object Layer (VOL)
 *
 * \subsection subsec_vol_intro Introduction
 * The virtual object layer is an abstraction layer in the HDF5 library that intercepts all API calls
 * that could potentially access objects in an HDF5 container and forwards those calls to a VOL
 * connector, which implements the storage. The user or application gets the benefit of using the
 * familiar and widely-used HDF5 data model and API, but can map the physical storage of the HDF5 file
 * and objects to storage that better meets the application’s data needs.
 *
 * \subsection subsec_vol_abstract_layer The VOL Abstraction Layer
 * The VOL lies just under the public API. When a storage-oriented public APIcall is made, the library
 * performs a few sanity checks on the input parameters and then immediately invokes a VOL callback,
 * which resolves to an implementation in the VOL connector that was selected when opening or creating
 * the file. The VOL connector then performs whatever operations are needed before control returns to the
 * library, where any final library operations such as assigning IDs for newly created/opened datasets are
 * performed before returning. This means that, for calls that utilize the VOL, all of the functionality
 * is deferred to the VOL connector and the HDF5 library does very little work. An important consideration
 * of this is that most of the HDF5 caching layers (metadata and chunk caches, page buffering, etc.) will
 * not be available as those are implemented in the HDF5 native VOL connector and cannot be easily reused
 * by external connectors.
 *
 * <table>
 * <tr>
 * <td>
 * \image html vol_architecture.png "The VOL Architecture"
 * </td>
 * </tr>
 * </table>
 *
 * Not all public HDF5 API calls pass through the VOL. Only calls which require manipulating storage go
 * through the VOL and require a VOL connector author to implement the appropriate callback. Dataspace,
 * property list, error stack, etc. calls have nothing to do with storage manipulation or querying and
 * do not use the VOL. This may be confusing when it comes to property list calls, since many of those
 * calls set properties for storage. Property lists are just collections of key-value pairs, though, so
 * a particular VOL connector is not required to set or get properties.
 *
 * Another thing to keep in mind is that not every VOL connector will implement the full HDF5 public API.
 * In some cases, a particular feature like variable-length types may not have been developed yet or may
 * not have an equivalent in the target storage system. Also, many HDF5 public API calls are specific to
 * the native HDF5 file format and are unlikely to have any use in other VOL connectors. A
 * feature/capabilities flag scheme is being developed to help navigate this.
 *
 * For more information about which calls go through the VOL and the mechanism by which this is implemented,
 * see the connector author and library internals documentation.
 *
 * \subsection subsec_vol_connect VOL Connectors
 * A VOL connector can be implemented in several ways:
 * \li as a shared or static library linked to an application
 * \li as a dynamically loaded plugin, implemented as a shared library
 * \li and even as an internal connector, built into the HDF5 library itself
 *
 * This section mostly focuses on external connectors, both libraries and plugins, as those are expected
 * to be much more common than internal implementations.
 *
 * A list of VOL connectors can be found here:
 * <a href="https://portal.hdfgroup.org/display/support/Registered+VOL+Connectors">
 * Registered VOL Connectors</a>
 *
 * This list is incomplete and only includes the VOL connectors that have been registered with
 * The HDF Group.
 *
 * Not every connector in this collection is actively maintained by The HDF Group. It simply serves as a
 * single location where important VOL connectors can be found. See the documentation in a connector's
 * repository to determine its development status and the parties responsible for it.
 *
 * A VOL template that contains build scripts (Autotools and CMake) and an empty VOL connector "shell"
 * which can be copied and used as a starting point for building new connectors is located here:
 * <a href="https://github.com/HDFGroup/vol-template">VOL Connector Template</a>
 *
 * This template VOL connector is for use in constructing terminal VOL connectors that do not forward
 * calls to an underlying connector. The external pass-through VOL connector listed on the registered
 * connector page can be used as a starting point for pass-through connectors.
 *
 * The only current (non-test) internal VOL connector distributed with the library is the native file
 * format connector (the "native VOL connector") which contains the code that handles native HDF5 (*.h5/hdf5)
 * files. In other words, even the canonical HDF5 file format is implemented via the VOL, making it a core
 * part of the HDF5 library and not an optional component which could be disabled.
 *
 * It has not been completely abstracted from the HDF5 library, though, and is treated as a special case.
 * For example, it cannot be unloaded and is always present.
 *
 * \section subsec_vol_use Connector Use
 *
 * Previous Chapter \ref sec_plist - Next Chapter \ref sec_map
 *
 */

/**
 *\defgroup H5VL VOL connector (H5VL)
 *
 * \todo Describe the VOL plugin life cycle.
 *
 * \defgroup ASYNC Asynchronous Functions
 * \brief List of the asynchronous functions.
 * \note The argument \p es_id associated with the asynchronous APIs is the \Emph{event set id}. See H5ES for
 *context.
 *
 * \defgroup H5VLDEF Definitions
 * \ingroup H5VL
 * \defgroup H5VLDEV VOL Developer
 * \ingroup H5VL
 * \defgroup H5VLNAT Native VOL
 * \ingroup H5VL
 * \defgroup H5VLPT Pass-through VOL
 * \ingroup H5VL
 */

#endif /* H5VLmodule_H */
