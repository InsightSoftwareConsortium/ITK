/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnchorUtilities.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkAnchorUtilities_h
#define __itkAnchorUtilities_h

#include <list>

#include "itkSharedMorphologyUtilities.h"

namespace itk
{
/** \class AnchorUtilities
* \brief functionality in common for anchor openings/closings and
* erosions/dilation
*
**/
#if defined( _MSC_VER ) && _MSC_VER >= 1300
#if 0
// can be moved to SharedMorphUtilities if user control of border is permitted
template< class TImage, class TBres, class TLine >
int FillLineBuffer(typename TImage::ConstPointer input,
                   const typename TImage::IndexType StartIndex,
                   const TLine line,
                   const float tol,
                   const typename TBres::OffsetArray LineOffsets,
                   const typename TImage::RegionType AllImage,
                   typename TImage::PixelType * inbuffer,
                   unsigned &start,
                   unsigned &end);
#endif

template< class TImage, class TBres, class TLine >
int ComputeStartEnd(const typename TImage::IndexType StartIndex,
                    const TLine line,
                    const float tol,
                    const typename TBres::OffsetArray LineOffsets,
                    const typename TImage::RegionType AllImage,
                    unsigned & start,
                    unsigned & end);

template< class TImage, class TBres, class TAnchor, class TLine >
void DoAnchorFace(const TImage * input,
                  TImage * output,
                  typename TImage::PixelType border,
                  TLine line,
                  TAnchor & AnchorLine,
                  typename TBres::OffsetArray LineOffsets,
                  typename TImage::PixelType * inbuffer,
                  typename TImage::PixelType * outbuffer,
                  const typename TImage::RegionType AllImage,
                  const typename TImage::RegionType face);

// This creates a list of non overlapping faces that need to be
// processed for this particular line orientation. We are doing this
// instead of using the Face Calculator to avoid repeated operations
// starting from corners.

//template <class TRegion, class TLine>
//std::list<TRegion> mkFaceList(const TRegion AllImage,
//                              const TLine line);

#endif
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAnchorUtilities.txx"
#endif

#endif
