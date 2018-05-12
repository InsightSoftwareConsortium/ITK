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
#ifndef itkBoneMorphometryFeaturesImageFilter_h
#define itkBoneMorphometryFeaturesImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNumericTraits.h"
#include "itkSimpleDataObjectDecorator.h"
#include "itksys/hash_map.hxx"
#include "itkHistogram.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkSimpleDataObjectDecorator.h"
#include "itkConstantBoundaryCondition.h"

#include <vector>

namespace itk
{
/** \class BoneMorphometryFeaturesImageFilter
 * \brief Compute the percent bone volume [BVTV], trabecular thickness [TbTh], trabecular separation [TbSp],
 * trabecular number [TbN] and Bone Surface to Bone Volume ration [BSBV] for each voxel of
 * a given image and a mask image if provided. The output image can then be  displayed by using colormaps.
 *
 * BoneMorphometryFeaturesImageFilter computes bone morphometry features maps. The filter is able to compute the following features:
 * -# the percent bone volume [BVTV]
 * -# the trabecular thickness [TbTh]
 * -# the trabecular separation [TbSp]
 * -# the trabecular number [TbN]
 * -# the Bone Surface to Bone Volume ration [BSBV].
 *
 * To do so, the filter needs:
 * -# a 3D input scan
 * -# an optional mask (the morphometry will be computed only for the mask's voxels with a value of 1)
 * -# a threshold (All voxels with an intensity higher than the threshold will be considered as part of the bone)
 * -# a neighborhood radius
 *
 *  * Recommendations:
 * -# Input image: To improve the computation time, the useful data should take as much
 *    space as possible in the input image. If the useful data is concentrated in one part of
 *    the image a crop step should be considered prior to the usage of this filter.
 * -# Mask: Even if optional, the usage of a mask will greatly improve the computation time.
 * -# Output: The filter output image will be either a vector image or an image containing vectors of 5 scalars
 *
 * \author: Jean-Baptiste Vimort
 * \ingroup BoneMorphometry
 *
 */
template< typename TInputImage,
          typename TOutputImage,
          typename TMaskImage = Image< unsigned char, TInputImage::ImageDimension> >
class ITK_TEMPLATE_EXPORT BoneMorphometryFeaturesImageFilter:
public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard Self typedef */
  typedef BoneMorphometryFeaturesImageFilter              Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage>  Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BoneMorphometryFeaturesImageFilter, ImageToImageFilter);

  /** Image related typedefs. */
  typedef typename TInputImage::Pointer    InputImagePointer;
  typedef typename TInputImage::RegionType RegionType;
  typedef typename TInputImage::SizeType   SizeType;
  typedef typename TInputImage::IndexType  IndexType;
  typedef typename TInputImage::PixelType  PixelType;

  /** Mask related typedefs. */
  typedef typename TMaskImage::Pointer    MaskImagePointer;

  /** Output related typedefs. */
  typedef typename TOutputImage::Pointer                            OutputImagePointer;
  typedef typename TOutputImage::RegionType                         OutputRegionType;
  typedef typename TOutputImage::PixelType                          OutputPixelType;
  typedef typename NumericTraits< OutputPixelType >::ScalarRealType OutputRealType;

  /** NeighborhoodIterator typedef */
  typedef ConstantBoundaryCondition< TInputImage >                           BoundaryConditionType;
  typedef ConstNeighborhoodIterator< TInputImage, BoundaryConditionType >    NeighborhoodIteratorType;
  typedef typename NeighborhoodIteratorType::RadiusType                      NeighborhoodRadiusType;
  typedef typename NeighborhoodIteratorType::OffsetType                      NeighborhoodOffsetType;
  typedef typename NeighborhoodIteratorType::NeighborIndexType               NeighborIndexType;

  /** Type to use for computations. */
  typedef typename NumericTraits< PixelType >::RealType RealType;

  /** Methods to set/get the mask image */
  itkSetInputMacro(MaskImage, TMaskImage);
  itkGetInputMacro(MaskImage, TMaskImage);

  /** Methods to set/get the threshold */
  itkSetMacro(Threshold, RealType);
  itkGetMacro(Threshold, RealType);

  /** Method to set/get the Neighborhood radius */
  itkSetMacro(NeighborhoodRadius, NeighborhoodRadiusType);
  itkGetConstMacro(NeighborhoodRadius, NeighborhoodRadiusType);

  /** Methods to get the mask different outputs */


#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputPixelDimensionCheck,
                   ( Concept::SameDimension<TInputImage::ImageDimension, 3u>) );
  // End concept checking
#endif

protected:
  BoneMorphometryFeaturesImageFilter();
  virtual ~BoneMorphometryFeaturesImageFilter() {}

  /** Do final mean and variance computation from data accumulated in threads.
    */
  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  /** Multi-thread version GenerateData. */
  virtual void  ThreadedGenerateData(const RegionType &
                                     outputRegionForThread,
                                     ThreadIdType threadId) ITK_OVERRIDE;

  bool IsInsideNeighborhood(const NeighborhoodOffsetType &iteratedOffset);
  bool IsInsideMaskRegion(const IndexType &imageIndex, const typename TMaskImage::SizeType &maskSize);


  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:

  //Inputs
  RealType                m_Threshold;
  NeighborhoodRadiusType  m_NeighborhoodRadius;

}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBoneMorphometryFeaturesImageFilter.hxx"
#endif

#endif // itkBoneMorphometryFeaturesImageFilter_h
