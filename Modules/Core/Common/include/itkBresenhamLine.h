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
#ifndef itkBresenhamLine_h
#define itkBresenhamLine_h

#include "itkVector.h"
#include "itkIndex.h"
#include <vector>

namespace itk
{
/* a simple class that will return an array of indexes that are
* offsets along the line. The line will be described by a vector and a
* length */

template< unsigned int VDimension >
class ITK_TEMPLATE_EXPORT BresenhamLine
{
public:
  typedef BresenhamLine Self;
  // This defines the line direction
  typedef Vector< float, VDimension > LType;
  typedef Offset< VDimension >        OffsetType;
  typedef Index< VDimension >         IndexType;
  typedef std::vector< OffsetType >   OffsetArray;
  typedef std::vector< IndexType >    IndexArray;

  // constructurs
  BresenhamLine(){}
  ~BresenhamLine(){}

  /** Build a line in a specified Direction. */
  OffsetArray BuildLine(LType Direction, unsigned int length);

  /** Build a line between two pixels. */
  IndexArray BuildLine(IndexType p0, IndexType p1);
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBresenhamLine.hxx"
#endif

#endif
