/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMObjectFactory.h
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
#ifndef __itkFEMFEMObjectFactory_h
#define __itkFEMFEMObjectFactory_h

#include <string>
#include <vector>

namespace itk {
namespace fem {




/**
 * \brief Create objects of derived classes by specifying a class ID.
 *
 * ObjectFactory is used to create various objects of derived classes by
 * specifying an ID of a derived class. Before the objects can be created by
 * object factory, you should first call the Register method for each class:
 *
 * int ID_Derived=FEMObjectFactory<BaseClass>.Register( NewDerivedClass, 
 *                                                      "NewDerivedClassName"
 *                                                    );
 *
 *  - BaseClass: Base class from which the registered classes are derived
 *
 *  - ID_Derived: Integer returned by the Register function that
 *                specifies the derived class' ID. Class ID's
 *                are assigned automatically.
 *
 *  - NewDerivedClass: Function that creates a new instance of a derived
 *                     class and returns a pointer to it as a pointer
 *                     to the base class. Normally you should define this
 *                     function as:
 *
 *                        - BaseClass* NewDerivedClass()
 *                              { return new DerivedClass; }
 *
 *  - NewDerivedClassname: String with a name of a derived class.
 *
 * You should also put the NewDerivedClass function in an anonymous namespace
 * and make it static. So a complete registration of a derived class
 * looks like:
 *
 *  - namespace { static Base* NewDerivedClass() { return new DerivedClass; } } \n
 *    const int OF_Derived=FEMObjectFactory<BaseClass>::Register( NewDerivedClass, "NewDerivedObjectName" );
 *
 */
template<class T>
class FEMObjectFactory
{

  /**
   * Type that holds a pointer to function which can create a new object of a derived class.
   */
  typedef typename T::Pointer (*COF)();

  /**
   * Type that holds class name as a string.
   */
  typedef std::string StrClassName;

  /**
   * Type that holds array of the above functions and class names.
   */
  typedef std::vector<std::pair<COF,StrClassName> > COF_Array;
  
public:

  /**
   * Create a new object based on class identifier id and return a pointer to it.
   */  
  static typename T::Pointer Create(int id) {
    return (Instance().cofs_[id].first)();
  }

  /**
   * Register the class with the factory. A pointer to a 'create'
   * function and class name as a string must be provided. Function
   * returns the newly assigned ID of the class, which can be later 
   * used to create objects of that class.
   */
  static int Register(COF f, const char *str)
  {
/*    std::cout<<"OF->"<<str<<"\n"; */
    Instance().cofs_.push_back( COF_Array::value_type(f,str) );
    return Instance().cofs_.size()-1;
  }

  /**
   * Return the name of the class (as a string) for the given ID.
   */
  static StrClassName ID2ClassName(int id)
  {
    return Instance().cofs_[id].second;
  }

  /**
   * Find the ID of the class with specified name (this is a slow function).
   * If you have to create many objects of the same class, obtain the class ID
   * with this function first and then use that ID with the Create member
   * function.
   */
  static int ClassName2ID(StrClassName str)
  {
    int j=0;
    for(COF_Array::const_iterator i=Instance().cofs_.begin(); i!=Instance().cofs_.end(); i++) {
      if (i->second==str) return j;
      j++;
    }
    return -1;
  }

private:

  /**
   * Array that stores pairs of create functions and class names.
   */
  COF_Array cofs_;

  /**
   * Private constructor. This class is implemented as a singleton, so we
   * don't allow anybody from outside to construct it.
   */
  FEMObjectFactory();

  /**
   * Private copy constructor.
   */
  FEMObjectFactory(const FEMObjectFactory&);

  /**
   * Private destructor.
   */
  ~FEMObjectFactory();

  /**
   * Access to the only instance of the FEMObjectFactory object.
   */
  inline static FEMObjectFactory& Instance();

  /**
   * Deletes the object in obj member. This function is
   * called when application finishes (atexit() function).
   */
  static void CleanUP();

  /**
   * Pointer to the only instance of the FEMObjectFactory class.
   */
  static FEMObjectFactory* obj;

private:
  /**
   * \brief This class is defined in FEMObjectFactory just to get rid of some
            warnings about destructor being private in gcc
   */
  class Dummy {};
  friend class Dummy;

};


template<class T>
FEMObjectFactory<T>* FEMObjectFactory<T>::obj = 0;

template<class T>
FEMObjectFactory<T>::FEMObjectFactory() {}

template<class T>
FEMObjectFactory<T>::FEMObjectFactory(const FEMObjectFactory<T>&) {}

template<class T>
FEMObjectFactory<T>::~FEMObjectFactory() {}

template<class T>
FEMObjectFactory<T>& FEMObjectFactory<T>::Instance() 
  {
  if (!obj) 
    { 
    /**
     * Create a new FEMObjectFactory object if we don't have it already.
     */
    obj=new FEMObjectFactory;

    /**
     * Make sure that the object that ws just created is also destroyed
     * when program finishes.
     */
    atexit(&CleanUP);
          
    }

  /**
   * Return the actual FEMObjectFactory object
   */
  return *obj;
  }

template<class T>
void FEMObjectFactory<T>::CleanUP() { delete obj; }




}} // end namespace itk::fem

#endif // #ifndef __itkFEMFEMObjectFactory_h
