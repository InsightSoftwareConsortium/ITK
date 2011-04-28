/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkFEMLightObject_h
#define __itkFEMLightObject_h

#include "itkFEMMacro.h"
#include "itkFEMException.h"
#include <iostream>

namespace itk {
namespace fem {

/**
 * \class FEMLightObject
 * \brief Base class for all classes that define the FEM system.
 *
 * Base class for all classes that define FEM system (Elements, Nodes...).
 * Every FEM object requires a global number that can be used to find
 * that object. It is also required that the object can be written to and
 * read from a file (stream). This functionality is implemented inside
 * FEMLightObject class.
 * \ingroup ITK-FEM
 */
class FEMLightObject
#ifdef FEM_USE_SMART_POINTERS
: public itk::LightObject
#endif
{
  /**
   * If we're not using smart pointers then we make the
   * the Superclass equal to FEMLightObject, just to be able
   * to use the FEM_ABSTRACT_CLASS macro.
   */
#ifndef FEM_USE_SMART_POINTERS
  FEM_ABSTRACT_CLASS(FEMLightObject,FEMLightObject)
#else
  /**
   * If we are using smart pointers, Superclass is itk::LightObject
   */
  FEM_ABSTRACT_CLASS(FEMLightObject,itk::LightObject)
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
   * Returns the class ID of the object. This function is used to determine
   * the class of the object without having to use the dynamic_cast operator.
   *
   * \note Class must be registered with the FEMObjectFactory in order
   *       to create the class ID. Abstract classes don't define this
   *       function.
   */
  virtual int ClassID() const = 0;

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
   * are, so that we can write the class name. The class name is obtained
   * by calling the virtual ClassID() member function and passing
   * the result to the FEMObjectFactory.
   *
   * Implementations of Write member funtion in derived classes should
   * first call the parent's implementation of Write and finaly write
   * whatever they need.
   */
  virtual void Write( std::ostream& f ) const;

  /**
   * Read object of any derived type from stream.
   *
   * This static function creates an object of a class, which is derived
   * from FEMLightObject. The class of object is first determined from the
   * stream, then the object of that class is constructed using the
   * FEMObjectFactory. Finally the data for this object is read from the
   * stream, by calling the Read() member function.
   */
  static FEMLightObject::Pointer CreateFromStream( std::istream& f, void *info );

  /**
   * Helper function that skips all the whitespace and comments in
   * an input stream.
   */
  static void SkipWhiteSpace( std::istream& f );

  /**
   * Const string of all whitespace characters. This string is used by
   * #SkipWhiteSpace function.
   */
  static const std::string whitespaces;


#ifdef FEM_USE_SMART_POINTERS
protected:  // If we're using smart pointers, constructors and destructors should be protected.
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


/**
 * Short alias for FEMObjectFactory<FEMLightObject>
 */
typedef FEMObjectFactory<FEMLightObject> FEMOF;


}} // end namespace itk::fem

#endif // #ifndef __itkFEMLightObject_h
