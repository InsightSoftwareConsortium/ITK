// C++ informative line for the emacs editor: -*- C++ -*-
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef __H5ObjCreatPropList_H
#define __H5ObjCreatPropList_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

//! Class ObjCreatPropList represents the HDF5 object creation property list.
class H5_DLLCPP ObjCreatPropList : public PropList {
   public:
	///\brief Default object creation property list.
	static const ObjCreatPropList& DEFAULT;

	// Creates a object creation property list.
	ObjCreatPropList();

	// Sets attribute storage phase change thresholds.
	void setAttrPhaseChange(unsigned max_compact = 8, unsigned min_dense = 6) const;

	// Gets attribute storage phase change thresholds.
	void getAttrPhaseChange(unsigned& max_compact, unsigned& min_dense) const;

	// Sets tracking and indexing of attribute creation order.
	void setAttrCrtOrder(unsigned crt_order_flags) const;

	// Gets tracking and indexing settings for attribute creation order.
	unsigned getAttrCrtOrder() const;


	///\brief Returns this class name.
	virtual H5std_string fromClass () const { return("ObjCreatPropList"); }

	// Copy constructor: creates a copy of a ObjCreatPropList object.
	ObjCreatPropList( const ObjCreatPropList& original );

	// Creates a copy of an existing object creation property list
	// using the property list id.
	ObjCreatPropList (const hid_t plist_id);

	// Noop destructor
	virtual ~ObjCreatPropList();

#ifndef DOXYGEN_SHOULD_SKIP_THIS

        // Deletes the global constant, should only be used by the library
        static void deleteConstants();

    private:
        static ObjCreatPropList* DEFAULT_;

        // Creates the global constant, should only be used by the library
        static ObjCreatPropList* getConstant();

#endif // DOXYGEN_SHOULD_SKIP_THIS

};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif // __H5ObjCreatPropList_H
