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
#ifndef __itkBloxBoundaryPointPixel_h
#define __itkBloxBoundaryPointPixel_h

#include "itkBloxBoundaryPointItem.h"
#include "itkBloxPixel.h"

namespace itk
{
/**
 * \class BloxBoundaryPointPixel
 * \brief Holds a linked list of itk::BloxBoundaryPointItem's.
 *
 * \ingroup ImageObjects
 *
 * \ingroup ITK-Blox
 */

template< unsigned int NDimensions >
class ITK_EXPORT BloxBoundaryPointPixel:public BloxPixel<
    BloxBoundaryPointItem< NDimensions > >
{
public:
  BloxBoundaryPointPixel();
  ~BloxBoundaryPointPixel();
};
} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_BloxBoundaryPointPixel(_, EXPORT, TypeX, TypeY)     \
  namespace itk                                                          \
  {                                                                      \
  _( 1 ( class EXPORT BloxBoundaryPointPixel< ITK_TEMPLATE_1 TypeX > ) ) \
  namespace Templates                                                    \
  {                                                                      \
  typedef BloxBoundaryPointPixel< ITK_TEMPLATE_1 TypeX >                 \
  BloxBoundaryPointPixel##TypeY;                                       \
  }                                                                      \
  }

#if ITK_TEMPLATE_EXPLICIT
#include "Templates/itkBloxBoundaryPointPixel+-.h"
#endif

#if ITK_TEMPLATE_TXX
#include "itkBloxBoundaryPointPixel.txx"
#endif

#endif
