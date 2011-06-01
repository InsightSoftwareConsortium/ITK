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
 * intended as a base class for all pixels stored in itk::BloxImage derived
 * classes.
 * A particular type of itk::BloxImage is fully specialized by setting the type
 * of itk::BloxPixel that it holds, so in some sense this is the most important
 * class in the blox hierarchy.
 *
 * It is assumed that particular itk::BloxPixel derived types will add
 * functionality to this base class; for example, eigenanalysis of core atom
 * populations in itk::BloxCoreAtomPixel
 *
 * \ingroup ImageObjects
 * \ingroup ITK-Blox
 */
template< typename TItemType >
class BloxPixel:public std::list< TItemType * >
{
public:

  /** Delete all entries in the list, then clear the list. */
  void DeleteListEntries();

  /** Get the number of items stored in the blox. */
  unsigned long int GetSize()
  { return static_cast< unsigned long >( this->size() ); }

  BloxPixel();
  ~BloxPixel();
};
} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_BloxPixel(_, EXPORT, TypeX, TypeY)     \
  namespace itk                                             \
  {                                                         \
  _( 1 ( class EXPORT BloxPixel< ITK_TEMPLATE_1 TypeX > ) ) \
  namespace Templates                                       \
  {                                                         \
  typedef BloxPixel< ITK_TEMPLATE_1 TypeX >                 \
  BloxPixel##TypeY;                                       \
  }                                                         \
  }

#if ITK_TEMPLATE_EXPLICIT
#include "Templates/itkBloxPixel+-.h"
#endif

#if ITK_TEMPLATE_TXX
#include "itkBloxPixel.txx"
#endif

#endif
