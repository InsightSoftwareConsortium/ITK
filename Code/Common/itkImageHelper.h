/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageHelper.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageHelper_h
#define __itkImageHelper_h

#include "itkImageBase.h"
#include "itkConceptChecking.h"

namespace itk
{

/** \class ImageHelper
 *  \brief Fast Index/Offset computation
 */
template <unsigned int NImageDimension, unsigned int NLoop>
class ImageHelper
{
public:
  typedef ImageBase<NImageDimension>         ImageType;
  typedef typename ImageType::IndexType      IndexType;
  typedef typename ImageType::OffsetType     OffsetType;
  typedef typename ImageType::IndexValueType     IndexValueType;
  typedef typename ImageType::OffsetValueType     OffsetValueType;
  typedef Concept::Detail::UniqueType_bool<false> UniqueTypeBoolFalse;
  typedef Concept::Detail::UniqueType_bool<true> UniqueTypeBoolTrue;

  // ComputeIndex
  //
  inline static void ComputeIndex(const IndexType &bufferedRegionIndex,
                                  OffsetValueType offset,
                                  const OffsetValueType offsetTable[],
                                  IndexType &index)
    {
      ImageHelper<NImageDimension,NLoop-1>::        
      ComputeIndexInner(bufferedRegionIndex,
                        offset,
                        offsetTable,
                        index,
                        Concept::Detail::UniqueType_bool<(NLoop==1)>());
    }

  inline static void ComputeIndexInner(const IndexType &bufferedRegionIndex,    
                                       OffsetValueType &offset,
                                       const OffsetValueType offsetTable[],
                                       IndexType &index,
                                       const UniqueTypeBoolFalse& )
  {
    index[NLoop] = static_cast<IndexValueType>(offset / offsetTable[NLoop]);
    offset = offset - (index[NLoop] * offsetTable[NLoop]);
    index[NLoop] = index[NLoop] + bufferedRegionIndex[NLoop];
    ImageHelper<NImageDimension, NLoop-1>::        
      ComputeIndexInner(bufferedRegionIndex,
                        offset,
                        offsetTable,
                        index,
                        Concept::Detail::UniqueType_bool<(NLoop==1)>());
    
  }

  inline static void ComputeIndexInner(const IndexType &bufferedRegionIndex,
                                       OffsetValueType &offset,
                                       const OffsetValueType offsetTable[],
                                       IndexType &index,
                                       const UniqueTypeBoolTrue& )
  {
    // Do last 
    index[0] = bufferedRegionIndex[0] + static_cast<IndexValueType>(offset);
  }
};
} // end namespace itk

#endif

