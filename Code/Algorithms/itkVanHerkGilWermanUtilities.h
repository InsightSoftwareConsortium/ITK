/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVanHerkGilWermanUtilities.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkVanHerkGilWermanUtilities_h
#define __itkVanHerkGilWermanUtilities_h

#include <list>

#include "itkSharedMorphologyUtilities.h"

namespace itk
{
/**
 * \class VanHerkGilWermanUtilities
 * \brief functionality in common for anchor openings/closings and
 * erosions/dilation
 *
 */
#if defined( _MSC_VER ) && _MSC_VER >= 1300
#if 0
// version with no user access to border
template< class TImage, class TBres, class TLine, class TFunction >
int FillLineBuffer(typename TImage::ConstPointer input,
                   const typename TImage::IndexType StartIndex,
                   typename TImage::PixelType border,
                   const TLine line,  // unit vector
                   const float tol,
                   const typename TBres::OffsetArray LineOffsets,
                   const typename TImage::RegionType AllImage,
                   const unsigned int KernLen,
                   typename TImage::PixelType * pixbuffer,
                   typename TImage::PixelType * fExtBuffer,
                   unsigned &start,
                   unsigned &end);
#else
template< class PixelType, class TFunction >
void FillReverseExt(PixelType *pixbuffer, PixelType *rExtBuffer,
                    const unsigned int KernLen, unsigned len);

template< class PixelType, class TFunction >
void FillForwardExt(PixelType *pixbuffer, PixelType *fExtBuffer,
                    const unsigned int KernLen, unsigned len);

#endif

template< class TImage, class TBres, class TFunction, class TLine >
void DoFace(typename TImage::ConstPointer input,
            typename TImage::Pointer output,
            typename TImage::PixelType border,
            TLine line,
            const typename TBres::OffsetArray LineOffsets,
            const unsigned int KernLen,
            typename TImage::PixelType * pixbuffer,
            typename TImage::PixelType * fExtBuffer,
            typename TImage::PixelType * rExtBuffer,
            const typename TImage::RegionType AllImage,
            const typename TImage::RegionType face);

#endif
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVanHerkGilWermanUtilities.txx"
#endif

#endif
