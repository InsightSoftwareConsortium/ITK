/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMP.h
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
#ifndef __itkFEMP_h
#define __itkFEMP_h

namespace itk {
namespace fem {




/**
 * \class FEMP
 * \brief Pointer used to store polymorphic elements in STL arrays
 *
 * FEMP holds a pointer to objects of class T and its derived classes. it behaves like 
 * a special kind of pointer. special pointers to object can be used to store 
 * polymorphic arrays in STL. the basic idea of the special pointer is:
 *    whatever you do to the pointer (object of class FEMP), is reflected
 *    on the object within (pointed to by data member).
 *
 * for example: if you copy the pointer, an object within is also copied
 *
 * the class T should have a member Clone() which produces a copy of an object. this is
 * important in polymorphic classes, where object of the derived class should be created when
 * copying an existing object.
 */
template<class T>
class FEMP
{
public:

  /**
   * default constructor makes sure that data is 0, to prevent problems when deleting data object on destruction
   */
  FEMP() : data(0){}      

  /**
   * copy constructor. we call the Clone() method to duplicate the existing object
   */  
  FEMP(const FEMP& x) 
  {      
    if (x.data) { data=static_cast<T*>(&*x.data->Clone()); }
    else { data=0; }
  }

  /**
   * conversion constructor from T* to FEMP<T>. the object T* must exist. we take ownership of object T*
   * if you want create a copy of object and take ownership of that, use: FEMP(x->Clone()) instead of FEMP(x).
   */  
  explicit FEMP(typename T::Pointer x) : data(x) {}

  /**
   * destructor of a special pointer class also destroys the actual object
   */
  ~FEMP()
  {
    #ifndef FEM_USE_SMART_POINTERS
    delete data;
    #endif
  }

  /**
   * asignment operator
   */  
  const FEMP& operator=(const FEMP &rhs);

  /**
   * for easy access of object members
   */
  typename T::Pointer operator-> () const { return data; }
  
  /**
   * dereferencing operator provides automatic conversion from special to standard pointer to object
   */  
  operator T * () const { return data; }  
  
  /**
   * returns true if special pointer actually points to a valid object and false otherwise
   */  
  bool IsNULL() const {          
    return (data==0); 
  }

private:

  /**
   * pointer to actual object
   */
  typename T::Pointer data;

};

template<class T>
const FEMP<T>& FEMP<T>::operator=(const FEMP<T> &rhs) 
  {

  /**
   * self assignments don't make sense
   */
  if (&rhs!=this) 
  {      
    /**
     * first destroy the existing object on the left hand side
     */
    #ifndef FEM_USE_SMART_POINTERS
    delete data;
    #else
    data=0;
    #endif

    /**
     * then clone the one on the right hand side of the expression (if not NULL)
     */
    if (rhs.data) { data=static_cast<T*>(&*rhs.data->Clone()); }
    else { data=0; }

  }
  return *this;
  }




}} // end namespace itk::fem

#endif // #ifndef __itkFEMP_h
