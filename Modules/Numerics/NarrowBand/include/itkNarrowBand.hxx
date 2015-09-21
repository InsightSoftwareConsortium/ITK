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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkNarrowBand_hxx
#define itkNarrowBand_hxx
#include "itkNarrowBand.h"
#include <cmath>

namespace itk
{
#if !defined( ITK_WRAPPING_PARSER )
template< typename NodeType >
std::vector< typename NarrowBand< NodeType >::RegionType >
NarrowBand< NodeType >
::SplitBand(const SizeType& n)
{
  SizeType t_n = n;
  SizeType t_size = m_NodeContainer.size();

  std::vector< RegionType > regionList;
  if ( t_n > t_size )
    {
    t_n = t_size;
    }

  SizeType regionsize =
    static_cast< SizeType >(
        std::floor( static_cast< float >( t_size ) / static_cast< float >( t_n ) ) );

  if ( regionsize == 0 )
    {
    regionsize = 1;
    }

  RegionType region;
  Iterator   pos = this->Begin();

  for ( SizeType i = 0; i < t_n; ++i )
    {
    region.Begin = pos;
    pos += regionsize;

    if ( i != t_n - 1 )
      {
      region.End = pos;
      }
    else
      {
      region.End = this->End();
      }

    regionList.push_back(region);
    }

  return regionList;
}

#endif
} // end namespace itk

#endif
