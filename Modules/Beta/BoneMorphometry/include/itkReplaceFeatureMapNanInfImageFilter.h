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
#ifndef itkReplaceFeatureMapNanInfImageFilter_h
#define itkReplaceFeatureMapNanInfImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNumericTraits.h"
#include "itkSimpleDataObjectDecorator.h"

//useful filters
#include <itkVectorIndexSelectionCastImageFilter.h>
#include <itkMinimumMaximumImageFilter.h>
#include <itkMaskImageFilter.h>
#include <itkComposeImageFilter.h>

#include <vector>

namespace itk
{
/** \class ReplaceFeatureMapNanInfImageFilter
 * \brief This new filter can be used after the usage of itkBoneMorphometryFeaturesImageFilter
 * in order to remove the Nan and Inf values of the feature maps. (Those values are due to
 * neighborhood containing only bone voxel, containing 0 bone voxel)
 *
 * This filter is working with two passes for each feature map:
 *   -The first pass allow the detection of the minumum and maximum values of the feature maps.
 *   -During the second pass, every NaN or Inf value will be replace by eather the maximum of minimum value detected
 *   in the first path  depending of the feature.
 *
 * \author: Jean-Baptiste Vimort
 * \ingroup BoneMorphometry
 *
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT ReplaceFeatureMapNanInfImageFilter:
public ImageToImageFilter< TImage, TImage >
{
public:
  /** Standard Self typedef */
  typedef ReplaceFeatureMapNanInfImageFilter    Self;
  typedef ImageToImageFilter< TImage, TImage>   Superclass;
  typedef SmartPointer< Self >                  Pointer;
  typedef SmartPointer< const Self >            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ReplaceFeatureMapNanInfImageFilter, ImageToImageFilter);

protected:

  /** Input Image related typedefs. */
  typedef typename TImage::Pointer           ImagePointer;
  typedef typename TImage::RegionType        RegionType;
  typedef typename TImage::SizeType          SizeType;
  typedef typename TImage::IndexType         IndexType;
  typedef typename TImage::PixelType         PixelType;

  /** Type to use for computations. */
  typedef typename NumericTraits< PixelType >::ScalarRealType RealType;

  /** Intermediate Image related typedefs. */
  typedef itk::Image< RealType, TImage::ImageDimension >        InterImageType;
  typedef itk::ImageRegionConstIterator< InterImageType >       InterIteratorType;

  ReplaceFeatureMapNanInfImageFilter();
  virtual ~ReplaceFeatureMapNanInfImageFilter() {}

  typedef VectorIndexSelectionCastImageFilter< TImage , InterImageType > IndexSelectionFiterType;
  typedef MinimumMaximumImageFilter< InterImageType >                    MinMaxImageFilterType;
  typedef MaskImageFilter< InterImageType , InterImageType >             MaskImageFilterType;

  void GenerateData() ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  typename  IndexSelectionFiterType::Pointer   m_IndexSelectionFiter;

}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkReplaceFeatureMapNanInfImageFilter.hxx"
#endif

#endif // itkReplaceFeatureMapNanInfImageFilter_h
