/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBloxPixel_h
#define __itkBloxPixel_h

#include <list>

#include "itkBloxItem.h"

namespace itk
{

/**
 * \class BloxPixel
 * \brief Holds a linked list of BloxItem's
 *
 * itk::BloxPixel is a specialized "value added" version of the basic STL list
 * intended as a base class for all pixels stored in itk::BloxImage derived classes.
 * A particular type of itk::BloxImage is fully specialized by setting the type
 * of itk::BloxPixel that it holds, so in some sense this is the most important
 * class in the blox hierarchy.
 *
 * It is assumed that particular itk::BloxPixel derived types will add functionality
 * to this base class; for example, eigenanalysis of core atom populations in
 * itk::BloxCoreAtomPixel
 *
 * \ingroup ImageObjects
 * */

template <typename TItemType>
class BloxPixel : public std::list<TItemType*>
{
public:

  /** Delete all entries in the list, then clear the list. */
  void DeleteListEntries();

  /** Get the number of items stored in the blox. */
  unsigned long int GetSize()
    {return static_cast<unsigned long>( this->size() ); }

  BloxPixel();
  ~BloxPixel();
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBloxPixel.txx"
#endif

#endif
