/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMPArray.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMPArray_h
#define __itkFEMPArray_h

#include "itkFEMP.h"
#include "itkFEMException.h"
#include <vector>

namespace itk {
namespace fem {




/**
 * \class FEMPArray
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
typename FEMPArray<T>::ClassTypePointer FEMPArray<T>::Find(int gn)
{

  typename Superclass::iterator i;

  /**
   * First take a guess. This only works on sorted
   * arrays and is much faster than searching.
   */
  if( gn<0 ||
      gn>=(int)size() ||
      ( *( i=static_cast<typename Superclass::iterator>(&this->operator[](gn)) ) )->GN!=gn )
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
typename FEMPArray<T>::ClassTypeConstPointer FEMPArray<T>::Find(int gn) const
{

  typedef typename Superclass::const_iterator ConstIterator;
  typedef typename Superclass::value_type     ValueType;

  const ValueType & value = this->operator[](gn);

  ConstIterator it   = this->begin(); 

  if( gn <   0 ||
      gn >= (int)size() ||
      value->GN != gn )
    {
    ConstIterator iend = this->end();
    while( it != iend )
      {
      if( (*it)->GN == gn )
        {
        break;
        }
      it++;
      }  

    if( it == this->end() )
      {
      throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"FEMPArray::Find() const",typeid(T).name(),gn);
      }

    }

  return &(*(*it));

}




template<class T>
int FEMPArray<T>::Renumber() 
{

  typename Superclass::iterator i;
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
