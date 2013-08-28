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
#ifndef __itkDemonsRegistrationDemonsFunction_h
#define __itkDemonsRegistrationDemonsFunction_h

#include "itkVariationalRegistrationFunction.h"

#include "itkCentralDifferenceImageFunction.h"

namespace itk
{

/** \class itk::VariationalRegistrationDemonsFunction
 *
 *  \sa VariationalRegistrationFunction
 *
 *  \ingroup FiniteDifferenceFunctions
 *  \ingroup VariationalRegistration
 */
template <class TFixedImage, class TMovingImage, class TDisplacementField>
class ITK_EXPORT VariationalRegistrationDemonsFunction
  : public VariationalRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>
{
public:
  /** Standard class typedefs. */
  typedef VariationalRegistrationDemonsFunction                                          Self;
  typedef VariationalRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField> Superclass;
  typedef SmartPointer<Self>                                                             Pointer;
  typedef SmartPointer<const Self>                                                       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VariationalRegistrationDemonsFunction, VariationalRegistrationFunction);

  /** Get image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** MovingImage image type. */
  typedef typename Superclass::MovingImageType    MovingImageType;
  typedef typename Superclass::MovingImagePointer MovingImagePointer;

  /** FixedImage image type. */
  typedef typename Superclass::FixedImageType    FixedImageType;
  typedef typename Superclass::FixedImagePointer FixedImagePointer;

  /** MaskImage image type. */
  typedef typename Superclass::MaskImageType    MaskImageType;
  typedef typename Superclass::MaskImagePointer MaskImagePointer;

  /** Image parameter types. */
  typedef typename FixedImageType::IndexType   IndexType;
  typedef typename FixedImageType::SizeType    SizeType;
  typedef typename FixedImageType::SpacingType SpacingType;

  /** Deformation field type. */
  typedef typename Superclass::DisplacementFieldType        DisplacementFieldType;
  typedef typename Superclass::DisplacementFieldTypePointer DisplacementFieldTypePointer;
  /** Various type definitions. */
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::FloatOffsetType  FloatOffsetType;

  /** Image gradient calculator type. */
  typedef CentralDifferenceImageFunction<FixedImageType> GradientCalculatorType;
  typedef typename GradientCalculatorType::Pointer       GradientCalculatorPointer;

  /** Set the object's state before each iteration. */
  virtual void
  InitializeIteration();

  /** This method is called by a finite difference solver image filter at
   * each pixel that does not lie on a data set boundary */
  virtual PixelType
  ComputeUpdate(const NeighborhoodType & neighborhood,
                void *                   globalData,
                const FloatOffsetType &  offset = FloatOffsetType(0.0));

  /** Select if the fixed image or warped image gradient is used for
   * computing the demon forces. The warped image gradient is used by default. */
  virtual void
  SetGradientTypeToFixedImage()
  {
    m_GradientType = GRADIENT_TYPE_FIXED;
  }

  virtual void
  SetGradientTypeToWarpedMovingImage()
  {
    m_GradientType = GRADIENT_TYPE_WARPED;
  }

  virtual void
  SetGradientTypeToSymmetric()
  {
    m_GradientType = GRADIENT_TYPE_SYMMETRIC;
  }

  /** Set/Get the threshold below which the absolute difference of
   * intensity yields a match. When the intensities match between a
   * moving and fixed image pixel, the update vector (for that
   * iteration) will be the zero vector. Default is 0.001. */
  virtual void
  SetIntensityDifferenceThreshold(const double threshold)
  {
    m_IntensityDifferenceThreshold = threshold;
  }
  virtual double
  GetIntensityDifferenceThreshold() const
  {
    return m_IntensityDifferenceThreshold;
  }

protected:
  VariationalRegistrationDemonsFunction();
  ~VariationalRegistrationDemonsFunction() {}

  typedef typename Superclass::GlobalDataStruct GlobalDataStruct;

  void
  PrintSelf(std::ostream & os, Indent indent) const;

  /** Type of available image forces */
  enum GradientType
  {
    GRADIENT_TYPE_WARPED = 0,
    GRADIENT_TYPE_FIXED = 1,
    GRADIENT_TYPE_SYMMETRIC = 2
  };

private:
  VariationalRegistrationDemonsFunction(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  /** Function to compute derivatives of the fixed image. */
  GradientCalculatorPointer m_FixedImageGradientCalculator;

  /** Function to compute derivatives of the warped image. */
  GradientCalculatorPointer m_WarpedImageGradientCalculator;

  /** Set if warped or fixed image gradient is used for force computation. */
  GradientType m_GradientType;

  /** Threshold below which the denominator term is considered zero. */
  double m_DenominatorThreshold;

  /** Threshold below which two intensity value are assumed to match. */
  double m_IntensityDifferenceThreshold;

  /** Precalculated normalizer for spacing consideration. */
  double m_Normalizer;

  /** Zero update return value (zero vector). */
  PixelType m_ZeroUpdateReturn;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalRegistrationDemonsFunction.hxx"
#endif

#endif
