/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodAlgorithm.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkNeighborhoodAlgorithm_h
#define __itkNeighborhoodAlgorithm_h

#include <list>
#include "itkImage.h"
#include "itkNeighborhood.h"
#include "itkNeighborhoodOperator.h"
#include "itkExceptionObject.h"
#include "itkNeighborhoodIterator.h"

namespace itk
{
namespace NeighborhoodAlgorithm
{
/** \class ImageBoundaryFacesCalculator */
template< class TImage >
struct ITK_EXPORT ImageBoundaryFacesCalculator {
  typedef typename NeighborhoodIterator< TImage >::RadiusType RadiusType;
  typedef typename TImage::RegionType                         RegionType;
  typedef typename TImage::IndexType                          IndexType;
  typedef typename IndexType::IndexValueType                  IndexValueType;
  typedef typename TImage::SizeType                           SizeType;
  typedef typename SizeType::SizeValueType                    SizeValueType;
  typedef std::list< RegionType >                             FaceListType;
  itkStaticConstMacro(ImageDimension, unsigned int, TImage::ImageDimension);

  FaceListType operator()(const TImage *, RegionType, RadiusType);
};

/** \class CalculateOutputWrapOffsetModifiers
 *
 * Helper class for setting up itkNeighborhoodIterator output
 * buffers. Calculates the necessary modifiers to synchronize input and output
 * iteration between images with equal RequestedRegion sizes but unequal
 * BufferedRegion sizes.
 */
template< class TImage >
struct ITK_EXPORT CalculateOutputWrapOffsetModifiers {
  typedef Offset< ::itk::GetImageDimension< TImage >::ImageDimension > OffsetType;
  OffsetType operator()(TImage *, TImage *) const;
};
} // end namespace NeighborhoodAlgorithm
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodAlgorithm.txx"
#endif

#endif
