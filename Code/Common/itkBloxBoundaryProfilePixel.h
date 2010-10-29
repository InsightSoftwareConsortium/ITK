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
#ifndef __itkBloxBoundaryProfilePixel_h
#define __itkBloxBoundaryProfilePixel_h

#include "itkBloxBoundaryProfileItem.h"
#include "itkPoint.h"
#include "itkBloxPixel.h"

namespace itk
{
template< unsigned int NDimensions >
class ITK_EXPORT BloxBoundaryProfilePixel:public BloxPixel<
    BloxBoundaryProfileItem< NDimensions > >
{
public:

  /** Run-time type information (and related methods). */
  itkTypeMacro(BloxBoundaryProfilePixel, BloxPixel);

  /** The type of boundary profile item we process. */
  typedef BloxBoundaryProfilePixel< NDimensions > BoundaryProfileItemType;

  /** The type of boundary point item we process. */
  typedef BloxBoundaryPointItem< NDimensions > BPItemType;

  /** The type used to store the position of the boundary point item. */
  typedef Point< double, NDimensions > PositionType;

  BloxBoundaryProfilePixel();
  virtual ~BloxBoundaryProfilePixel();
};
} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_BloxBoundaryProfilePixel(_, EXPORT, TypeX, TypeY)     \
  namespace itk                                                            \
  {                                                                        \
  _( 1 ( class EXPORT BloxBoundaryProfilePixel< ITK_TEMPLATE_1 TypeX > ) ) \
  namespace Templates                                                      \
  {                                                                        \
  typedef BloxBoundaryProfilePixel< ITK_TEMPLATE_1 TypeX >                 \
  BloxBoundaryProfilePixel##TypeY;                                       \
  }                                                                        \
  }

#if ITK_TEMPLATE_EXPLICIT
#include "Templates/itkBloxBoundaryProfilePixel+-.h"
#endif

#if ITK_TEMPLATE_TXX
#include "itkBloxBoundaryProfilePixel.txx"
#endif

#endif
