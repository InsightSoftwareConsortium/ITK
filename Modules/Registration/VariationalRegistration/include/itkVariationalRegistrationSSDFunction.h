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
#ifndef __itkVariationalRegistrationSSDFunction_h
#define __itkVariationalRegistrationSSDFunction_h

#include "itkVariationalRegistrationFunction.h"

#include "itkCentralDifferenceImageFunction.h"

namespace itk
{

/** \class itk::VariationalRegistrationSSDFunction
 *
 *  \brief This class computes SSD forces in the variational registration framework.
 *
 *  This class implements SSD forces given by
 *  \f[
 *    f^{SSD}(x)=\tau\kappa(F(x)-M(x+u(x)))\nabla M(x+u(x))
 *  \f]
 *  \f$\tau\f$ is the step size and \f$\kappa\f$ is the mean squared spacing.
 *  Alternative, the classical gradient \f$\nabla M(x+u(x))\f$ can be replaced by \f$\nabla F(x)\f$
 *  or \f$\frac{\nabla F(x) + \nabla M(x+u(x))}{2}\f$.
 *
 *  \sa VariationalRegistrationFilter
 *  \sa VariationalRegistrationFunction
 *
 *  \ingroup FiniteDifferenceFunctions
 *  \ingroup VariationalRegistration
 *
 *  \note This class was developed with funding from the German Research
 *  Foundation (DFG: EH 224/3-1 and HA 235/9-1).
 *  \author Alexander Schmidt-Richberg
 *  \author Rene Werner
 *  \author Jan Ehrhardt
 */
template <class TFixedImage, class TMovingImage, class TDisplacementField>
class ITK_EXPORT VariationalRegistrationSSDFunction
  : public VariationalRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>
{
public:
  /** Standard class typedefs. */
  typedef VariationalRegistrationSSDFunction                                             Self;
  typedef VariationalRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField> Superclass;
  typedef SmartPointer<Self>                                                             Pointer;
  typedef SmartPointer<const Self>                                                       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VariationalRegistrationSSDFunction, VariationalRegistrationFunction);

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
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::FloatOffsetType  FloatOffsetType;

  /** Image gradient calculator type. */
  typedef CentralDifferenceImageFunction<FixedImageType> GradientCalculatorType;
  typedef typename GradientCalculatorType::Pointer       GradientCalculatorPointer;

  /** Set the object's state before each iteration. */
  virtual void
  InitializeIteration() ITK_OVERRIDE;

  /** This method is called by a finite difference solver image filter at
   * each pixel that does not lie on a data set boundary */
  virtual PixelType
  ComputeUpdate(const NeighborhoodType & neighborhood,
                void *                   globalData,
                const FloatOffsetType &  offset = FloatOffsetType(0.0)) ITK_OVERRIDE;

  /** Select that the fixed image gradient is used for computing the forces. */
  virtual void
  SetGradientTypeToFixedImage()
  {
    m_GradientType = GRADIENT_TYPE_FIXED;
  }

  /** Select that the warped image gradient is used for computing the forces. */
  virtual void
  SetGradientTypeToWarpedMovingImage()
  {
    m_GradientType = GRADIENT_TYPE_WARPED;
  }

  /** Select that fixed and warped image gradients are used for computing the
   *  forces. */
  virtual void
  SetGradientTypeToSymmetric()
  {
    m_GradientType = GRADIENT_TYPE_SYMMETRIC;
  }

  /** Set the threshold below which the absolute difference of
   * intensity yields a match. When the intensities match between a
   * moving and fixed image pixel, the update vector (for that
   * iteration) will be the zero vector. Default is 0.001. */
  virtual void
  SetIntensityDifferenceThreshold(const double threshold)
  {
    m_IntensityDifferenceThreshold = threshold;
  }

  /** Get the threshold below which the absolute difference of
   * intensity yields a match. When the intensities match between a
   * moving and fixed image pixel, the update vector (for that
   * iteration) will be the zero vector. */
  virtual double
  GetIntensityDifferenceThreshold() const
  {
    return m_IntensityDifferenceThreshold;
  }

  /** Computes the time step for an update.
   * Returns the constant time step scaled with the mean squared spacing.
   * \sa SetTimeStep() */
  virtual typename Superclass::TimeStepType
  ComputeGlobalTimeStep(void * itkNotUsed(GlobalData)) const ITK_OVERRIDE
  {
    return this->GetTimeStep() * m_Normalizer;
  }

protected:
  VariationalRegistrationSSDFunction();
  ~VariationalRegistrationSSDFunction() {}

  typedef typename Superclass::GlobalDataStruct GlobalDataStruct;

  /** Print information about the filter. */
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Type of available image forces */
  enum GradientType
  {
    GRADIENT_TYPE_WARPED = 0,
    GRADIENT_TYPE_FIXED = 1,
    GRADIENT_TYPE_SYMMETRIC = 2
  };

private:
  VariationalRegistrationSSDFunction(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  /** Function to compute derivatives of the fixed image. */
  GradientCalculatorPointer m_FixedImageGradientCalculator;

  /** Function to compute derivatives of the warped image. */
  GradientCalculatorPointer m_WarpedImageGradientCalculator;

  /** Set if warped or fixed image gradient is used for force computation. */
  GradientType m_GradientType;

  /** Threshold below which two intensity value are assumed to match. */
  double m_IntensityDifferenceThreshold;

  /** Precalculated normalizer for spacing consideration. */
  double m_Normalizer;

  /** Zero update return value (zero vector). */
  PixelType m_ZeroUpdateReturn;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalRegistrationSSDFunction.hxx"
#endif

#endif
