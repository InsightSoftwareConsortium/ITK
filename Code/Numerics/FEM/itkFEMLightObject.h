/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLightObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/

#ifndef __itkFEMLightObject_h
#define __itkFEMLightObject_h

#include <typeinfo.h>
#include "itkFEMException.h"
#include "itkFEMMacro.h"

namespace itk {
namespace fem {


class LoadElement;

/**
 * \class FEMLightObject
 * \brief Base class for all classes that define FEM system.
 * Base class for all classes that define FEM system (Elements, Nodes...).
 * Every FEM object requires a global number that can be used to find
 * that object. It is also required that the object can be written to and
 * read from a file (stream). This functionality is implemented inside
 * FEMLightObject class.
 */
class FEMLightObject
#ifdef FEM_USE_SMART_POINTERS
: public ::itk::LightObject
#endif
{
  /**
   * If we're not using smart pointers then we make the
   * the Superclass equal to FEMLightObject, just to be able
   * to use the FEM_CLASS_SP macro.
   */
#ifndef FEM_USE_SMART_POINTERS
  FEM_CLASS_SP(FEMLightObject,FEMLightObject)
#else
  /**
   * If we are using smart pointers, Superclass is ::itk::LightObject
   */
  FEM_CLASS_SP(FEMLightObject,::itk::LightObject)
#endif

public:
  /**
   * Store the base class typedef for easy access from derived classes.
   * FEM_CLASS macro also expects this for the FEMOF...
   */
  typedef Self Baseclass;

  /**
   * Duplicates the currect object. This function must be implemented
   * by every derived class to create an exact copy of an object. The
   * function returns a pointer to a base class.
   */
  virtual Baseclass::Pointer Clone() const = 0;

  /**
   * Read an object data from input stream. Call this member to
   * initialize the data members in the current object by reading
   * data from provided input stream. Derived classes should first call
   * the the parent's read function, to initialize the data from parent.
   * Note that you must manually create the object of desired type
   * using the FEMObjectFactory before you can call read function (this
   * is pretty obvious). In this class only the global number
   * is read from file.
   * Derived classes may require some additional info in order to
   * perform the reading. Pack this info in an object and
   * pass a pointer to it in the info parameter. If you need runtime
   * typechecking, use a polymorphic class and dynamic_cast operator
   * inside the implementation of Read.
   */
  virtual void Read( std::istream& f, void* info );

  /**
   * Write an object to the output stream. Call this member to write
   * the data members in the current object to the output stream.
   * Here we also need to know which derived class we actually
   * are, so that we can write the class name. This is done by passing
   * the ObjectFactory id in ofid from the most derived class.
   *
   * Implementations of Write member funtion in derived classes should
   * first sheck if ofid is negative. If it is, they should set it to
   * the correct id of a class. If it is not, they should just leave it
   * as it is.
   * Next thing to do is call the parent's implementation of Write (make
   * sure that you also pass the ofid parameter), and finaly write
   * whatever you need.
   *
   * When you are calling this member to write the class to ostream, you
   * should omit the ofid member. It will be automatically set
   * correctly in the most derived implementation of Write.
   */
  virtual void Write( std::ostream& f, int ofid=-1 ) const;


#ifdef FEM_USE_SMART_POINTERS
protected:  /** If we're using smart pointers, constructors and destructors should be protected. */
#endif
  /**
   * Default constructor
   */
  FEMLightObject() : GN(-1) {}

  /**
   * Virtual destructor
   */
  virtual ~FEMLightObject() {}

  /**
   * Copy constructor must be available for the FEM objects...
   */
  FEMLightObject(const FEMLightObject& o) { GN=o.GN; }


public:
  /**
   * Global number of an object (ID of an object)
   * In general the ID's are required to be unique only within
   * a specific type of derived classes (Elements, Nodes, ...)
   * If the GN is not required, it can be ignored. (normally you
   * need the GN when writing or reading objects to/from stream.
   */
  int GN;

};


}} // end namespace itk::fem

#endif // #ifndef __itkFEMLightObject_h
