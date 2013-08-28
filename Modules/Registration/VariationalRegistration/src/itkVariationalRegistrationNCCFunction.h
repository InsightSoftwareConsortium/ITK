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
#ifndef __itkVariationalRegistrationNCCFunction_h
#define __itkVariationalRegistrationNCCFunction_h

// #include "itkPDEDeformableRegistrationFunction.h"
// #include "itkPoint.h"
#include "itkVariationalRegistrationFunction.h"
#include "itkCovariantVector.h"
#include "itkInterpolateImageFunction.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkCentralDifferenceImageFunction.h"

#include "itkNCCRegistrationFunction.h"


namespace itk
{

/**
 * \class VariationalRegistrationNCCFunction
 *
 * This class encapsulate the PDE which drives the registration
 * with normalized cross correlation (NCC). It is used by VariationalRegistrationFilter to compute the
 * output deformation field which will map a moving image onto a
 * a fixed image.
 *
 * The derivative of NCC is computed as given in Hermosillo et al., IJCV 50(3), 2002
 * and Avants et al., Med Image Anal 12(1), 2008 (except Jacobian term).
 *
 * This class is templated over the fixed image type, moving image type,
 * and the deformation field type.
 *
 * \warning This filter assumes that the fixed image type, moving image type
 * and deformation field type all have the same number of dimensions.
 *
 *  \sa VariationalRegistrationFunction
 *
 *  \ingroup VariationalRegistration
 */
template <class TFixedImage, class TMovingImage, class TDisplacementField>
class ITK_EXPORT VariationalRegistrationNCCFunction
  : public VariationalRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>
{
public:
  /** Standard class typedefs. */
  typedef VariationalRegistrationNCCFunction                                             Self;
  typedef VariationalRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField> Superclass;
  typedef SmartPointer<Self>                                                             Pointer;
  typedef SmartPointer<const Self>                                                       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VariationalRegistrationNCCFunction, VariationalRegistrationFunction);

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

  typedef typename FixedImageType::IndexType   IndexType;
  typedef typename FixedImageType::SizeType    SizeType;
  typedef typename FixedImageType::SpacingType SpacingType;

  /** Deformation field type. */
  typedef typename Superclass::DisplacementFieldType        DisplacementFieldType;
  typedef typename Superclass::DisplacementFieldTypePointer DisplacementFieldTypePointer;

  /** Inherit some types from the superclass. */
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::FloatOffsetType  FloatOffsetType;

  /** Gradient calculator type. */
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
   * computing the forces. The fixed image gradient is used by default. */
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

  /** Set/Get if global time step is scaled. If the flag is off, m_TimeStep is
   * returned during ComputeGlobalTimeStep(). If the flag is on (default), m_Timestep is
   * divided by the Mean squared spacing. */
  virtual void
  SetScaleGlobalTimeStep(const bool flag)
  {
    m_ScaleGlobalTimeStep = flag;
  }
  virtual bool
  GetScaleGlobalTimeStep() const
  {
    return m_ScaleGlobalTimeStep;
  }

  /** Depending on m_ScaleGlobalTimeStep, this class either simply uses the
   * constant time step m_TimeStep, or scales m_TimeStep with
   * 1 / Mean squared spacing.*/
  virtual typename Superclass::TimeStepType
  ComputeGlobalTimeStep(void * gd) const;

protected:
  VariationalRegistrationNCCFunction();
  ~VariationalRegistrationNCCFunction() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const;

  /** FixedImage image neighborhood iterator type. */
  typedef ConstNeighborhoodIterator<FixedImageType> FixedImageNeighborhoodIteratorType;

  typedef typename Superclass::GlobalDataStruct GlobalDataStruct;

  /** Type of available image forces */
  enum GradientType
  {
    GRADIENT_TYPE_WARPED = 0,
    GRADIENT_TYPE_FIXED = 1,
    GRADIENT_TYPE_SYMMETRIC = 2
  };

private:
  VariationalRegistrationNCCFunction(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  /** Function to compute derivatives of the fixed image. */
  GradientCalculatorPointer m_FixedImageGradientCalculator;

  /** Function to compute derivatives of the warped image. */
  GradientCalculatorPointer m_WarpedImageGradientCalculator;

  /** Set if warped or fixed image gradient is used for force computation. */
  GradientType m_GradientType;

  /** Precalculated normalizer for spacing consideration. */
  double m_Normalizer;

  /** Scale timestep with mean squared fixed image spacing. */
  bool m_ScaleGlobalTimeStep;
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalRegistrationNCCFunction.hxx"
#endif

#endif
