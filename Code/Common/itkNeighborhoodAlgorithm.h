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
#ifndef __itkNeighborhoodAlgorithm_h
#define __itkNeighborhoodAlgorithm_h

#include <list>
#include "itkImage.h"
#include "itkNeighborhoodOperator.h"
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
