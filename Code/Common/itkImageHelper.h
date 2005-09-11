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

#include "itkConceptChecking.h"

namespace itk
{

/** \class ImageHelper
 *  \brief Fast Index/Offset computation
 *
 * These helper methods use recursive templates to unroll the loops
 * of simple calculations. The resulting speed improvement varies from
 * compiler to compiler. Some gcc compilers with debug turned on
 * exhibit slight speed increases, but most compilers see
 * improvement. The ComputeOffset performance improvement is
 * impressive. For example, the Windows VS7.0 compiler shows almost a
 * factor of 2 speed improvement with the recursive templates. Usually
 * recursive templates use partial specialization to terminate
 * loops. Here we use a technique used by Brad King in the itk Concept
 * Checking code. 
 *
 * \note This work is part of the National Alliance for Medical Image
 * Computing (NAMIC), funded by the National Institutes of Health
 * through the NIH Roadmap for Medical Research, Grant U54 EB005149.
 * Information on the National Centers for Biomedical Computing
 * can be obtained from http://nihroadmap.nih.gov/bioinformatics. 
 *
 */

// Forward reference ImageBase
template <
  unsigned int NImageDimension
  > class ImageBase; 

template <unsigned int NImageDimension, unsigned int NLoop>
class ImageHelper
{
public:
  typedef ImageBase<NImageDimension>              ImageType;
  typedef typename ImageType::IndexType           IndexType;
  typedef typename ImageType::OffsetType          OffsetType;
  typedef typename ImageType::IndexValueType      IndexValueType;
  typedef typename ImageType::OffsetValueType     OffsetValueType;
  typedef Concept::Detail::UniqueType_bool<false> UniqueTypeBoolFalse;
  typedef Concept::Detail::UniqueType_bool<true>  UniqueTypeBoolTrue;

  /** ComputeIndex with recursive templates */
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
                                       const OffsetValueType [],
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
    offset = offset + (index[NLoop] - bufferedRegionIndex[NLoop])*offsetTable[NLoop];
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
                                        const OffsetValueType [],
                                        OffsetValueType &offset,
                                        const UniqueTypeBoolTrue& )
  {
    // Do last 
    offset = offset + index[0] - bufferedRegionIndex[0];
//    std::cout << "    offset += (index[0] - bufferedRegionIndex[0]);" << std::endl;
  }

};
} // end namespace itk

#endif

