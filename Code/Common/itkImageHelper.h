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
    offset -= (index[NLoop] * offsetTable[NLoop]);
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

  // ComputeOffset
  //
  inline static void ComputeOffset(const IndexType &bufferedRegionIndex,
                                   const IndexType &index,
                                   const OffsetValueType offsetTable[],
                                   OffsetValueType &offset)
    {
      offset = 0;
//      std::cout << "      offset = 0;" << std::endl;
      ImageHelper<NImageDimension,NLoop-1>::        
      ComputeOffsetInner(bufferedRegionIndex,
                        index,
                        offsetTable,
                        offset,
                        Concept::Detail::UniqueType_bool<(NLoop==1)>());
    }

  inline static void ComputeOffsetInner(const IndexType &bufferedRegionIndex,    
                                       const IndexType &index,
                                       const OffsetValueType offsetTable[],
                                       OffsetValueType &offset,
                                       const UniqueTypeBoolFalse& )
  {
    offset += (index[NLoop] - bufferedRegionIndex[NLoop])*offsetTable[NLoop];
//    std::cout << "    offset += (index[" << NLoop << "] - bufferedRegionIndex[" << NLoop << "])*offsetTable[" << NLoop << "];" << std::endl;
    ImageHelper<NImageDimension, NLoop-1>::        
      ComputeOffsetInner(bufferedRegionIndex,
                        index,
                        offsetTable,
                        offset,
                        Concept::Detail::UniqueType_bool<(NLoop==1)>());
    
  }

  inline static void ComputeOffsetInner(const IndexType &bufferedRegionIndex,
                                        const IndexType &index,
                                        const OffsetValueType offsetTable[],
                                        OffsetValueType &offset,
                                        const UniqueTypeBoolTrue& )
  {
    // Do last 
    offset += (index[0] - bufferedRegionIndex[0]);
//    std::cout << "    offset += (index[0] - bufferedRegionIndex[0]);" << std::endl;
  }

};
} // end namespace itk

#endif

