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

#ifndef itkEigenToMeasureImageFilter_h
#define itkEigenToMeasureImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkSimpleDataObjectDecorator.h"
#include "itkSpatialObject.h"

namespace itk {
/** \class EigenToMeasureImageFilter
 * \brief Abstract class for computing a measure from local structure.
 * 
 * This is an abstract class that computes a local-structure measure from an eigen-image.
 * Any algorithm implementing a local-structure measure should inherit from this class
 * so they can be used in the MultiScaleHessianEnhancementImageFilter framework.
 * 
 * \sa MultiScaleHessianEnhancementImageFilter
 * \sa EigenToMeasureParameterEstimationFilter
 * 
 * \author: Bryce Besler
 * \ingroup BoneEnhancement
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT EigenToMeasureImageFilter
  : public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(EigenToMeasureImageFilter);

  /** Standard Self typedef */
  using Self          = EigenToMeasureImageFilter;
  using Superclass    = ImageToImageFilter< TInputImage, TOutputImage >;
  using Pointer       = SmartPointer< Self >;
  using ConstPointer  = SmartPointer< const Self >;

  /** Run-time type information (and related methods). */
  itkTypeMacro(EigenToMeasureImageFilter, ImageToImageFilter);

  /** Input Image typedefs. */
  using InputImageType          = TInputImage;
  using InputImagePointer       = typename InputImageType::Pointer;
  using InputImageConstPointer  = typename InputImageType::ConstPointer;
  using InputImageRegionType    = typename InputImageType::RegionType;
  using InputImagePixelType     = typename InputImageType::PixelType;
  using PixelValueType          = typename InputImagePixelType::ValueType;
  itkStaticConstMacro(ImageDimension, unsigned int,  TInputImage::ImageDimension);

  /** Output image typedefs. */
  using OutputImageType       = TOutputImage;
  using OutputImagePointer    = typename OutputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType  = typename OutputImageType::PixelType;

  /** Input Mask typedefs. */
  using MaskSpatialObjectType               = SpatialObject< Self::ImageDimension >;
  using MaskSpatialObjectTypeConstPointer   = typename MaskSpatialObjectType::ConstPointer;

  /** Parameter typedefs. */
  using RealType                = typename NumericTraits< PixelValueType >::RealType;
  using ParameterType           = RealType;
  using ParameterArrayType      = Array< ParameterType >;
  using ParameterDecoratedType  = SimpleDataObjectDecorator< ParameterArrayType >;

  /** Process object */
  itkSetGetDecoratedInputMacro(Parameters, ParameterArrayType);

  /** Methods to set/get the mask image */
  itkSetInputMacro(Mask, MaskSpatialObjectType);
  itkGetInputMacro(Mask, MaskSpatialObjectType);

  /** Template the EigenValueOrderType. Methods that inherit from this class can override this function
   * to produce a different eigenvalue ordering. Ideally, the enum EigenValueOrderType should come from
   * itkSymmetricEigenAnalysisImageFilter.h or itkSymmetricEigenAnalysis.h. That turns out to be non-trivial
   * because the enumeration is hidden within the templated class. Therefore, you would need the hessian type
   * and eigenvalue type to do such an operation. We do not necessarily have the hessian type information.
   */
  typedef enum {
    OrderByValue = 1,
    OrderByMagnitude,
    DoNotOrder
  } EigenValueOrderType;
  virtual EigenValueOrderType GetEigenValueOrder() const = 0;

protected:
  EigenToMeasureImageFilter() {};
  virtual ~EigenToMeasureImageFilter() {}

  virtual OutputImagePixelType ProcessPixel(const InputImagePixelType& pixel) = 0;

  /** Multi-thread version GenerateData. */
  void DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;
}; // end class
} /* end namespace */

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEigenToMeasureImageFilter.hxx"
#endif

#endif /* itkEigenToMeasureImageFilter_h */
