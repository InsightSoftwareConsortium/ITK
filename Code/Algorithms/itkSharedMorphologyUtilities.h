/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSharedMorphologyUtilities.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkSharedMorphologyUtilities_h
#define __itkSharedMorphologyUtilities_h

#include <list>

namespace itk
{
#if defined( _MSC_VER ) && _MSC_VER >= 1300
template< class TRegion, class TLine >
bool NeedToDoFace(const TRegion AllImage,
                  const TRegion face,
                  const TLine line);

template< class TImage, class TBres, class TLine >
int ComputeStartEnd(const typename TImage::IndexType StartIndex,
                    const TLine line,
                    const float tol,
                    const typename TBres::OffsetArray LineOffsets,
                    const typename TImage::RegionType AllImage,
                    unsigned & start,
                    unsigned & end);

template< class TImage, class TBres, class TLine >
int FillLineBuffer(typename TImage::ConstPointer input,
                   const typename TImage::IndexType StartIndex,
                   const TLine line,
                   const float tol,
                   const typename TBres::OffsetArray LineOffsets,
                   const typename TImage::RegionType AllImage,
                   typename TImage::PixelType * inbuffer,
                   unsigned int &start,
                   unsigned int &end);

template< class TImage, class TBres >
void CopyLineToImage(const typename TImage::Pointer output,
                     const typename TImage::IndexType StartIndex,
                     const typename TBres::OffsetArray LineOffsets,
                     const typename TImage::PixelType *outbuffer,
                     const unsigned start,
                     const unsigned end);

// This returns a face with a normal between +/- 45 degrees of the
// line. The face is enlarged so that AllImage is entirely filled by
// lines starting from every pixel in the face. This means that some
// of the region will not touch the image. This approach is necessary
// because we want to be able to sweep the lines in a fashion that
// does not have overlap between them.
template< class TInputImage, class TLine >
typename TInputImage::RegionType
MakeEnlargedFace(const TInputImage *input,
                 const typename TInputImage::RegionType AllImage,
                 const TLine line);

// figure out the correction factor for length->pixel count based on
// line angle
template< class TLine >
unsigned int GetLinePixels(const TLine line);

#endif
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSharedMorphologyUtilities.txx"
#endif

#endif
