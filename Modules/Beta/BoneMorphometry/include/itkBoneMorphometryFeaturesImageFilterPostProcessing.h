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
#ifndef itkBoneMorphometryFeaturesImageFilterPostProcessing_h
#define itkBoneMorphometryFeaturesImageFilterPostProcessing_h

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
/** \class BoneMorphometryFeaturesImageFilterPostProcessing
 * \brief
 *
 * \author: Jean-Baptiste Vimort
 * \ingroup BoneMorphometry
 *
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT BoneMorphometryFeaturesImageFilterPostProcessing:
public ImageToImageFilter< TImage, TImage >
{
public:
  /** Standard Self typedef */
  typedef BoneMorphometryFeaturesImageFilterPostProcessing Self;
  typedef ImageToImageFilter< TImage, TImage>              Superclass;
  typedef SmartPointer< Self >                             Pointer;
  typedef SmartPointer< const Self >                       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BoneMorphometryFeaturesImageFilterPostProcessing, ImageToImageFilter);

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

  BoneMorphometryFeaturesImageFilterPostProcessing();
  virtual ~BoneMorphometryFeaturesImageFilterPostProcessing() {}

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
#include "itkBoneMorphometryFeaturesImageFilterPostProcessing.hxx"
#endif

#endif // itkBoneMorphometryFeaturesImageFilterPostProcessing_h
