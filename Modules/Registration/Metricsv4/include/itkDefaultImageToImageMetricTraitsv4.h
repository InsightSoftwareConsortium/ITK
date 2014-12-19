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
#ifndef itkDefaultImageToImageMetricTraitsv4_h
#define itkDefaultImageToImageMetricTraitsv4_h

#include "itkImage.h"
#include "itkCovariantVector.h"
#include "itkCentralDifferenceImageFunction.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkObjectToObjectMetricBase.h"

namespace itk
{
/** \class DefaultImageToImageMetricTraitsv4
 * \brief A simple structure holding type information for ImageToImageMetricv4 classes
 *
 * This class provides default type information for class members and methods
 * used in gradient calculation. This class is used for all images with
 * scalar pixel types. For images with vector pixel types, see
 * itkVectorImageToImageMetricTraitsv4.
 *
 * \sa itkVectorImageToImageMetricTraitsv4
 *
 * \ingroup ITKMetricsv4
 */
template<
  typename    TFixedImageType,
  typename    TMovingImageType,
  typename    TVirtualImageType,
  typename TCoordRep = double
  >
class DefaultImageToImageMetricTraitsv4
{
public:
  /** Standard class typedefs. */
  typedef DefaultImageToImageMetricTraitsv4 Self;

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

  typedef   CovariantVector< CoordinateRepresentationType,
                             itkGetStaticConstMacro(FixedImageDimension) >
                                                      FixedImageGradientType;
  typedef   CovariantVector< CoordinateRepresentationType,
                             itkGetStaticConstMacro(MovingImageDimension) >
                                                      MovingImageGradientType;

  typedef   CovariantVector< CoordinateRepresentationType,
                             itkGetStaticConstMacro(VirtualImageDimension) >
                                                      VirtualImageGradientType;

  typedef FixedImageGradientType   FixedImageComponentGradientType;
  typedef MovingImageGradientType  MovingImageComponentGradientType;
  typedef VirtualImageGradientType VirtualImageComponentGradientType;

  typedef DefaultConvertPixelTraits< FixedImageGradientType >  FixedImageGradientConvertType;
  typedef DefaultConvertPixelTraits< MovingImageGradientType > MovingImageGradientConvertType;

  /** Type of the filter used to calculate the gradients. */
  typedef typename NumericTraits< FixedImagePixelType >::RealType
                                                    FixedRealType;
  typedef CovariantVector< FixedRealType,
                           itkGetStaticConstMacro(FixedImageDimension) >
                                                    FixedGradientPixelType;
  typedef Image< FixedGradientPixelType,
                 itkGetStaticConstMacro(FixedImageDimension) >
                                                FixedImageGradientImageType;

  typedef ImageToImageFilter< FixedImageType, FixedImageGradientImageType >
                                                 FixedImageGradientFilterType;

  typedef typename NumericTraits< MovingImagePixelType >::RealType
                                                 MovingRealType;
  typedef CovariantVector< MovingRealType,
                           itkGetStaticConstMacro(MovingImageDimension) >
                                                 MovingGradientPixelType;
  typedef Image< MovingGradientPixelType,
                 itkGetStaticConstMacro(MovingImageDimension) >
                                                    MovingImageGradientImageType;

  typedef ImageToImageFilter< MovingImageType, MovingImageGradientImageType >
                                                 MovingImageGradientFilterType;

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
                        CovariantVector<double,
                                  itkGetStaticConstMacro( FixedImageDimension )>,
                        CoordinateRepresentationType>
                                            FixedImageGradientCalculatorType;
  typedef ImageFunction<MovingImageType,
                        CovariantVector<double,
                                  itkGetStaticConstMacro( MovingImageDimension )>,
                        CoordinateRepresentationType>
                                            MovingImageGradientCalculatorType;

  typedef CentralDifferenceImageFunction<FixedImageType,
                                         CoordinateRepresentationType>
                                          DefaultFixedImageGradientCalculator;
  typedef CentralDifferenceImageFunction<MovingImageType,
                                         CoordinateRepresentationType>
                                          DefaultMovingImageGradientCalculator;

  /** Only floating-point images are currently supported. To support integer images,
   * several small changes must be made to use an internal floating-point type for
   * computations rather than the image pixel type itself. */
  #ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro( OnlyDefinedForFloatingPointTypes0, ( itk::Concept::IsFloatingPoint<FixedRealType> ) );
  itkConceptMacro( OnlyDefinedForFloatingPointTypes1, ( itk::Concept::IsFloatingPoint<MovingRealType> ) );
  #endif // ITK_USE_CONCEPT_CHECKING
};
} // end namespace itk

//#ifndef ITK_MANUAL_INSTANTIATION
//#include "itkDefaultImageToImageMetricTraitsv4.hxx"
//#endif

#endif
