/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMPArray.h
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

#ifndef __itkFEMPArray_h
#define __itkFEMPArray_h

#include "itkFEMP.h"
#include "itkFEMException.h"
#include <vector>

namespace itk {
namespace fem {




/**
 * \brief Array for FEMP objects
 * 
 * Array class that holds special pointers to objects. Every object must contain
 * a variable member of type int called GN (global number). You can search for
 * an object with specific GN within an array.
 */
template<class T>
class FEMPArray : public std::vector<FEMP<T> >
{
public:
  /**
   * Standard Self typedef
   */
  typedef FEMPArray Self;

  /**
   * Standard Superclass typedef
   */
  typedef std::vector<FEMP<T> > Superclass;

  /**
   * Dumb pointer typedef support.
   */
  typedef Self* Pointer;
  typedef const Self* ConstPointer;

  /**
   * Easy (and required on MSVC) access to the base class of objects inside the array.
   */
  typedef T ClassType;
  typedef typename ClassType::Pointer ClassTypePointer;
  typedef typename ClassType::ConstPointer ClassTypeConstPointer;

  /**
   * Finds and returns a pointer to the object with specific global number
   */
  ClassTypePointer Find(int gn);
  ClassTypeConstPointer Find(int gn) const;

  /**
   * Returns a pointer to i-th object stored in an array (not a pointer to FEMP of that object).
   */
  ClassTypePointer operator() (int i) 
  {
    return &(*operator[](i));
  }

  /**
   * Returns a pointer to i-th object stored in an array (not a pointer to FEMP of that object).
   * This function works on the const arrays.
   */
  ClassTypeConstPointer operator() (int i) const 
  {  
    return &(*operator[](i));
  }


  /**
   * Applies new numbers to objects in array so that they are in order (0,1,2,...).
   * This speeds up finding object by global number a lot. The function returns
   * total number of objects in an array.
   */
  int Renumber();

};



/**
 * Find function for for non-const objects
 */
template<class T>
FEMPArray<T>::ClassTypePointer FEMPArray<T>::Find(int gn)
{

  Superclass::iterator i;

  /**
   * First take a guess. This only works on sorted
   * arrays and is much faster than searching.
   */
  if( gn<0 ||
      gn>=(int)size() ||
      ( *( i=static_cast<Superclass::iterator>(&this->operator[](gn)) ) )->GN!=gn )
  {
    /** 
     * The array is not sorted, we need to search for the correct GN.
     */
    for(i=begin(); i!=end() && (*i)->GN!=gn; i++);

    /**
     * We din't find an object with that GN...
     */
    if(i==end())
    {
      throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"FEMPArray::Find()",typeid(T).name(),gn);
    }
  }

  /**
   * Return a pointer to the found object.
   */
  return &(*(*i));

}




/**
 * Find function for for const objects
 */
template<class T>
FEMPArray<T>::ClassTypeConstPointer FEMPArray<T>::Find(int gn) const
{

  Superclass::const_iterator i;

  if( gn<0 ||
      gn>=(int)size() ||
      ( *( i=static_cast<Superclass::const_iterator>(&this->operator[](gn)) ) )->GN!=gn )
  {
    for(i=begin(); i!=end() && (*i)->GN!=gn; i++);
    if(i==end())
    {
      throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"FEMPArray::Find() const",typeid(T).name(),gn);
    }

  }

  return &(*(*i));

}




template<class T>
int FEMPArray<T>::Renumber() 
{

  Superclass::iterator i;
  int j=0;

  for(i=begin(); i!=end(); i++)
  {
    (*i)->GN=j;
    j++;
  }

  return j;

}




}} // end namespace itk::fem

#endif // #ifndef __itkFEMPArray_h
