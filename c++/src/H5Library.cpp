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

#include <string>
#include <cstdlib>

#include "H5CppDoc.h" // included only for Doxygen to generate part of RM
#include "H5Include.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"
#include "H5FaccProp.h"
#include "H5FcreatProp.h"
#include "H5OcreatProp.h"
#include "H5DxferProp.h"
#include "H5DcreatProp.h"
#include "H5LcreatProp.h"
#include "H5LaccProp.h"
#include "H5DaccProp.h"
#include "H5Location.h"
#include "H5Object.h"
#include "H5DataType.h"
#include "H5AtomType.h"
#include "H5PredType.h"
#include "H5DataSpace.h"
#include "H5Library.h"

namespace H5 {

//--------------------------------------------------------------------------
// Function:    H5Library::open (static)
///\brief       Initializes the HDF5 library.
///
///\exception   H5::LibraryIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void
H5Library::open()
{
    herr_t ret_value = H5open();
    if (ret_value < 0) {
        throw LibraryIException("H5Library::open", "H5open failed");
    }
}

//--------------------------------------------------------------------------
// Function:    H5Library::close (static)
///\brief       Flushes all data to disk, closes files, and cleans up memory.
///
///\exception   H5::LibraryIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void
H5Library::close()
{
    herr_t ret_value = H5close();
    if (ret_value < 0) {
        throw LibraryIException("H5Library::close", "H5close failed");
    }
}

//--------------------------------------------------------------------------
// Function:    H5Library::dontAtExit (static)
///\brief       Instructs library not to install the C \c atexit cleanup routine
///
///\exception   H5::LibraryIException
// Programmer   Binh-Minh Ribler - 2000
// Modification
//              Removed the check for failure returned from H5dont_atexit.
//              will be fixed to not fail (HDFFV-9540)
//--------------------------------------------------------------------------
void
H5Library::dontAtExit()
{
    (void)H5dont_atexit();
}

//--------------------------------------------------------------------------
// Function:    H5Library::getLibVersion (static)
///\brief       Returns the HDF library release number.
///\param       majnum - OUT: Major version of the library
///\param       minnum - OUT: Minor version of the library
///\param       relnum - OUT: Release number of the library
///\exception   H5::LibraryIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void
H5Library::getLibVersion(unsigned &majnum, unsigned &minnum, unsigned &relnum)
{
    herr_t ret_value = H5get_libversion(&majnum, &minnum, &relnum);
    if (ret_value < 0) {
        throw LibraryIException("H5Library::getLibVersion", "H5get_libversion failed");
    }
}

//--------------------------------------------------------------------------
// Function:    H5Library::checkVersion (static)
///\brief       Verifies that the arguments match the version numbers
///             compiled into the library
///\param       majnum - IN: Major version of the library
///\param       minnum - IN: Minor version of the library
///\param       relnum - IN: Release number of the library
///\exception   H5::LibraryIException
///\par Description
///             For information about library version, please refer to
///             the H5check_version API in the HDF5 C Reference Manual.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void
H5Library::checkVersion(unsigned majnum, unsigned minnum, unsigned relnum)
{
    herr_t ret_value = H5check_version(majnum, minnum, relnum);
    if (ret_value < 0) {
        throw LibraryIException("H5Library::checkVersion", "H5check_version failed");
    }
}

//--------------------------------------------------------------------------
// Function:    H5Library::garbageCollect (static)
///\brief       Walks through all the garbage collection routines for the
///             library, which are supposed to free any unused memory they
///             have allocated.
///
///\exception   H5::LibraryIException
///\par Description
///             It is not required that H5Library::garbageCollect be called
///             at any particular time; it is only necessary in certain
///             situations, such as when the application has performed actions
///             that cause the library to allocate many objects. The
///             application should call H5Library::garbageCollect if it
///             eventually releases those objects and wants to reduce the
///             memory used by the library from the peak usage required.
///\par
///             The library automatically garbage collects all the free
///             lists when the application ends.
// Programmer   Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
void
H5Library::garbageCollect()
{
    herr_t ret_value = H5garbage_collect();
    if (ret_value < 0) {
        throw LibraryIException("H5Library::garbageCollect", "H5garbage_collect failed");
    }
}

//--------------------------------------------------------------------------
// Function:    H5Library::initH5cpp (static)
///\brief       Initializes C++ library and registers terminating functions at
///             exit.  Only for the library functions, not for user-defined
///             functions.
// Description
//              initH5cpp registers the following functions with std::atexit():
//                      termH5cpp() - calls H5close() after all cleanup in
//                                    the C++ library is done
//                      <classname>::deleteConstants - deletes all references
//                                    for <classname> global constants
///\exception   H5::LibraryIException
//
// Programmer   Binh-Minh Ribler - September, 2015
//--------------------------------------------------------------------------
void
H5Library::initH5cpp()
{
    // Register terminating functions with atexit(); they will be invoked in
    // the reversed order
    int ret_value = 0;
    ret_value     = std::atexit(termH5cpp);
    if (ret_value != 0)
        throw LibraryIException("H5Library::initH5cpp", "Registering termH5cpp failed");

    ret_value = std::atexit(PredType::deleteConstants);
    if (ret_value != 0)
        throw LibraryIException("H5Library::initH5cpp", "Registering PredType::deleteConstants failed");

    ret_value = std::atexit(PropList::deleteConstants);
    if (ret_value != 0)
        throw LibraryIException("H5Library::initH5cpp", "Registering PropList::deleteConstants failed");

    ret_value = std::atexit(DSetAccPropList::deleteConstants);
    if (ret_value != 0)
        throw LibraryIException("H5Library::initH5cpp",
                                "Registering DSetAccPropList::deleteConstants failed");

    ret_value = std::atexit(LinkAccPropList::deleteConstants);
    if (ret_value != 0)
        throw LibraryIException("H5Library::initH5cpp",
                                "Registering LinkAccPropList::deleteConstants failed");

    ret_value = std::atexit(LinkCreatPropList::deleteConstants);
    if (ret_value != 0)
        throw LibraryIException("H5Library::initH5cpp",
                                "Registering LinkCreatPropList::deleteConstants failed");

    ret_value = std::atexit(FileAccPropList::deleteConstants);
    if (ret_value != 0)
        throw LibraryIException("H5Library::initH5cpp",
                                "Registering FileAccPropList::deleteConstants failed");

    ret_value = std::atexit(FileCreatPropList::deleteConstants);
    if (ret_value != 0)
        throw LibraryIException("H5Library::initH5cpp",
                                "Registering FileCreatPropList::deleteConstants failed");

    ret_value = std::atexit(DSetMemXferPropList::deleteConstants);
    if (ret_value != 0)
        throw LibraryIException("H5Library::initH5cpp",
                                "Registering DSetMemXferPropList::deleteConstants failed");

    ret_value = std::atexit(DSetCreatPropList::deleteConstants);
    if (ret_value != 0)
        throw LibraryIException("H5Library::initH5cpp",
                                "Registering DSetCreatPropList::deleteConstants failed");

    ret_value = std::atexit(ObjCreatPropList::deleteConstants);
    if (ret_value != 0)
        throw LibraryIException("H5Library::initH5cpp",
                                "Registering ObjCreatPropList::deleteConstants failed");

    ret_value = std::atexit(DataSpace::deleteConstants);
    if (ret_value != 0)
        throw LibraryIException("H5Library::initH5cpp", "Registering DataSpace::deleteConstants failed");
}

//--------------------------------------------------------------------------
// Function:    H5Library::termH5cpp (static)
///\brief       Sends request for the C layer to terminate.
///\par Description
///             If the C library fails to terminate, exit with a failure.
// Programmer   Binh-Minh Ribler - September, 2015
//--------------------------------------------------------------------------
void
H5Library::termH5cpp()
{
    // Close the C library
    herr_t ret_value = H5close();
    if (ret_value == -1)
        exit(-1);
}

//--------------------------------------------------------------------------
// Function:    H5Library::setFreeListLimits (static)
///\brief       Sets limits on the different kinds of free lists.
///\param       reg_global_lim - IN: Limit on all "regular" free list memory used
///\param       reg_list_lim   - IN: Limit on memory used in each "regular" free list
///\param       arr_global_lim - IN: Limit on all "array" free list memory used
///\param       arr_list_lim   - IN: Limit on memory used in each "array" free list
///\param       blk_global_lim - IN: Limit on all "block" free list memory used
///\param       blk_list_lim   - IN: Limit on memory used in each "block" free list
///\exception   H5::LibraryIException
///\par Description
///             Setting a value of -1 for a limit means no limit of that type.
///             For more information on free list limits, please refer to
///             the H5set_free_list_limits API in the HDF5 C Reference Manual.
// Programmer   Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
void
H5Library::setFreeListLimits(int reg_global_lim, int reg_list_lim, int arr_global_lim, int arr_list_lim,
                             int blk_global_lim, int blk_list_lim)
{
    herr_t ret_value = H5set_free_list_limits(reg_global_lim, reg_list_lim, arr_global_lim, arr_list_lim,
                                              blk_global_lim, blk_list_lim);
    if (ret_value < 0) {
        throw LibraryIException("H5Library::setFreeListLimits", "H5set_free_list_limits failed");
    }
}

#ifndef DOXYGEN_SHOULD_SKIP_THIS
//--------------------------------------------------------------------------
// Function:    H5Library default constructor - private
///\brief       Default constructor: Creates a stub H5Library object
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
H5Library::H5Library()
{
}

//--------------------------------------------------------------------------
// Function:    H5Library destructor
///\brief       Noop destructor
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
H5Library::~H5Library()
{
}
#endif // DOXYGEN_SHOULD_SKIP_THIS

} // namespace H5
