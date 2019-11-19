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

#ifndef itkEigenToMeasureParameterEstimationFilter_h
#define itkEigenToMeasureParameterEstimationFilter_h

#include "itkStreamingImageFilter.h"
#include "itkSimpleDataObjectDecorator.h"
#include "itkSpatialObject.h"

namespace itk {
/** \class EigenToMeasureParameterEstimationFilter
 * \brief Abstract class for estimating local-structure parameters from an eigen-image.
 *
 * This is an abstract class that estimates parameters for local-structure enhancement
 * from eigen-images. Any algorithm implementing a local-structure measure should inherit from this class
 * so they can be used in the MultiScaleHessianEnhancementImageFilter framework.
 * 
 * To estimate parameters for local-structure enhancement, every pixel in the input
 * image must be seen. To implement this functionality in a memory efficinet way, this
 * class inherits from itk::StreamingImageFilter. This algorithm can be spead up by
 * restricting the output image region.
 * 
 * The method GetParametersOutput can be used to insert this filter in a pipeline before
 * EigenToMeasureImageFilter.
 * 
 * \sa StreamingImageFilter
 * \sa MultiScaleHessianEnhancementImageFilter
 * \sa EigenToMeasureImageFilter
 * 
 * \author: Bryce Besler
 * \ingroup BoneEnhancement
 */
template< typename TInputImage, typename TOutputImage = TInputImage >
class ITK_TEMPLATE_EXPORT EigenToMeasureParameterEstimationFilter
  : public StreamingImageFilter< TInputImage, TOutputImage >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(EigenToMeasureParameterEstimationFilter);

  /** Standard Self typedef */
  using Self          = EigenToMeasureParameterEstimationFilter;
  using Superclass    = StreamingImageFilter< TInputImage, TOutputImage >;
  using Pointer       = SmartPointer< Self >;
  using ConstPointer  = SmartPointer< const Self >;

  /** Run-time type information (and related methods). */
  itkTypeMacro(EigenToMeasureParameterEstimationFilter, StreamingImageFilter);

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
  using OutputImageRegionType = typename OutputImageType::RegionType;

  /** Input Mask typedefs. */
  using MaskSpatialObjectType               = SpatialObject< Self::ImageDimension >;
  using MaskSpatialObjectTypeConstPointer   = typename MaskSpatialObjectType::ConstPointer;

  /** Parameter typedefs. */
  using RealType                = typename NumericTraits< PixelValueType >::RealType;
  using ParameterType           = RealType;
  using ParameterArrayType      = Array< ParameterType >;
  using ParameterDecoratedType  = SimpleDataObjectDecorator< ParameterArrayType >;

  /** Decorators for parameters so they can be passed as a process object */
  ParameterDecoratedType * GetParametersOutput();
  const ParameterDecoratedType * GetParametersOutput() const;

  /** Standard getters for the parameters */
  ParameterArrayType GetParameters() const
  {
    return this->GetParametersOutput()->Get();
  }

  /** Methods to set/get the mask image */
  itkSetInputMacro(Mask, MaskSpatialObjectType);
  itkGetInputMacro(Mask, MaskSpatialObjectType);

  /** Override UpdateOutputData() from StreamingImageFilter to divide
   * upstream updates into pieces. This filter does not have a GenerateData()
   * or ThreadedGenerateData() method.  Instead, all the work is done
   * in UpdateOutputData() since it must update a little, execute a little,
   * update some more, execute some more, etc. */
  void UpdateOutputData(DataObject *output) override;

protected:
  EigenToMeasureParameterEstimationFilter();
  virtual ~EigenToMeasureParameterEstimationFilter() {}

  void PrintSelf(std::ostream & os, Indent indent) const override;
}; //end class
} // end namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEigenToMeasureParameterEstimationFilter.hxx"
#endif

#endif // itkEigenToMeasureParameterEstimationFilter_h
