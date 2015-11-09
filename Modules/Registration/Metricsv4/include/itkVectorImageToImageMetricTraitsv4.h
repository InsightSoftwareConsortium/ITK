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
#ifndef itkVectorImageToImageMetricTraitsv4_h
#define itkVectorImageToImageMetricTraitsv4_h

#include "itkImage.h"
#include "itkCovariantVector.h"
#include "itkObjectToObjectMetricBase.h"
#include "itkDefaultConvertPixelTraits.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkCentralDifferenceImageFunction.h"

namespace itk
{
/** \class VectorImageToImageMetricTraitsv4
 * \brief A simple structure holding type information for ImageToImageMetricv4 classes
 *
 * This class provides type information for class members and methods
 * used in gradient calculation. This class is used for images with
 * vector pixel types, including VectorImage. For images with scalar pixel types, see
 * itkDefaultImageToImageMetricTraitsv4.
 *
 * \sa itkDefaultImageToImageMetricTraitsv4
 *
 * \ingroup ITKMetricsv4
 */
template<
  typename    TFixedImageType,
  typename    TMovingImageType,
  typename    TVirtualImageType,
  unsigned int NumberOfComponents,
  typename TCoordRep = typename ObjectToObjectMetricBase::CoordinateRepresentationType
  >
class VectorImageToImageMetricTraitsv4
{
public:
  /** Standard class typedefs. */
  typedef VectorImageToImageMetricTraitsv4 Self;

  typedef TFixedImageType   FixedImageType;
  typedef TMovingImageType  MovingImageType;
  typedef TVirtualImageType VirtualImageType;

  typedef typename FixedImageType::PixelType  FixedImagePixelType;
  typedef typename MovingImageType::PixelType MovingImagePixelType;

  typedef TCoordRep            CoordinateRepresentationType;

  /* Image dimension accessors */
  typedef unsigned int   ImageDimensionType;
  itkStaticConstMacro(FixedImageDimension, ImageDimensionType,
      FixedImageType::ImageDimension);
  itkStaticConstMacro(MovingImageDimension, ImageDimensionType,
      MovingImageType::ImageDimension);
  itkStaticConstMacro(VirtualImageDimension, ImageDimensionType,
      VirtualImageType::ImageDimension);

  typedef Vector< CoordinateRepresentationType, FixedImageDimension*NumberOfComponents >   FixedImageGradientType;
  typedef Vector< CoordinateRepresentationType, MovingImageDimension*NumberOfComponents >  MovingImageGradientType;
  typedef Vector< CoordinateRepresentationType, VirtualImageDimension*NumberOfComponents > VirtualImageGradientType;

  typedef DefaultConvertPixelTraits< FixedImageGradientType >  FixedImageGradientConvertType;
  typedef DefaultConvertPixelTraits< MovingImageGradientType > MovingImageGradientConvertType;

  /** Type of the filter used to calculate the gradients. */
  typedef typename NumericTraits< FixedImagePixelType >::RealType    FixedRealType;
  typedef typename NumericTraits< MovingImagePixelType >::RealType   MovingRealType;

  typedef FixedImageGradientType                               FixedGradientPixelType;
  typedef MovingImageGradientType                              MovingGradientPixelType;

  typedef Image< FixedGradientPixelType,
                 itkGetStaticConstMacro(FixedImageDimension) >
                                                FixedImageGradientImageType;

  typedef ImageToImageFilter< FixedImageType, FixedImageGradientImageType >
                                                 FixedImageGradientFilterType;

  typedef Image< MovingGradientPixelType,
                 itkGetStaticConstMacro(MovingImageDimension) >
                                                    MovingImageGradientImageType;

  typedef ImageToImageFilter< MovingImageType, MovingImageGradientImageType >
                                                 MovingImageGradientFilterType;

  typedef CovariantVector<CoordinateRepresentationType, FixedImageDimension>    FixedImageComponentGradientType;
  typedef CovariantVector<CoordinateRepresentationType, MovingImageDimension>   MovingImageComponentGradientType;
  typedef CovariantVector<CoordinateRepresentationType, VirtualImageDimension>  VirtualImageComponentGradientType;

  /** Default image gradient filter types */
  typedef GradientRecursiveGaussianImageFilter< FixedImageType,
                                                FixedImageGradientImageType >
                                                  DefaultFixedImageGradientFilter;
  typedef GradientRecursiveGaussianImageFilter< MovingImageType,
                                                MovingImageGradientImageType >
                                                  DefaultMovingImageGradientFilter;

  /** Image gradient calculator types. The TOutput template parameter
   * is chosen to match that of CentralDiffererenceImageFunction. */
  typedef ImageFunction<FixedImageType,
                        FixedImageGradientType,
                        CoordinateRepresentationType>
                                            FixedImageGradientCalculatorType;
  typedef ImageFunction<MovingImageType,
                        MovingImageGradientType,
                        CoordinateRepresentationType>
                                            MovingImageGradientCalculatorType;

  typedef CentralDifferenceImageFunction<FixedImageType,
                                         CoordinateRepresentationType,
                                         FixedImageGradientType>
                                          DefaultFixedImageGradientCalculator;
  typedef CentralDifferenceImageFunction<MovingImageType,
                                         CoordinateRepresentationType,
                                         MovingImageGradientType>
                                          DefaultMovingImageGradientCalculator;
};
} // end namespace itk

#endif
