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
#include "itkSpatialObject.h"
#include "itkSimpleDataObjectDecorator.h"

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
template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject >
class ITK_TEMPLATE_EXPORT EigenToMeasureImageFilter:
public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard Self typedef */
  typedef EigenToMeasureImageFilter                       Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(EigenToMeasureImageFilter, ImageToImageFilter);

  /** Input Image typedefs. */
  typedef TInputImage                             InputImageType;
  typedef typename InputImageType::Pointer        InputImagePointer;
  typedef typename InputImageType::ConstPointer   InputImageConstPointer;
  typedef typename InputImageType::RegionType     InputImageRegionType;
  typedef typename InputImageType::PixelType      InputImagePixelType;
  typedef typename InputImagePixelType::ValueType PixelValueType;
  itkStaticConstMacro(ImageDimension, unsigned int,  TInputImage::ImageDimension);

  /** Output image typedefs. */
  typedef TOutputImage                          OutputImageType;
  typedef typename OutputImageType::Pointer     OutputImagePointer;
  typedef typename OutputImageType::RegionType  OutputImageRegionType;
  typedef typename OutputImageType::PixelType   OutputImagePixelType;

  /** Input SpatialObject typedefs. */
  typedef TInputSpatialObject                       SpatialObjectType;
  typedef typename SpatialObjectType::ConstPointer  SpatialObjectConstPointer;

  /** Parameter typedefs. */
  typedef typename NumericTraits< PixelValueType >::RealType  RealType;
  typedef RealType                                            ParameterType;
  typedef Array< ParameterType >                              ParameterArrayType;
  typedef SimpleDataObjectDecorator< ParameterArrayType >     ParameterDecoratedType;

  /** Process object */
  itkSetGetDecoratedInputMacro(Parameters, ParameterArrayType);

  /** Methods to set/get the mask image */
  itkSetInputMacro(MaskingSpatialObject, SpatialObjectType);
  itkGetInputMacro(MaskingSpatialObject, SpatialObjectType);

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
private:
  ITK_DISALLOW_COPY_AND_ASSIGN(EigenToMeasureImageFilter);
}; // end class
} /* end namespace */

#endif /* itkEigenToMeasureImageFilter_h */
