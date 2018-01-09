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
#ifndef itkMIRegistrationFunction_h
#define itkMIRegistrationFunction_h

#include "itkPDEDeformableRegistrationFunction.h"
#include "itkPoint.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkCentralDifferenceImageFunction.h"

namespace itk
{
/**
 * \class MIRegistrationFunction
 *
 * This class encapsulates a mutual information force for deformable registration
 * algorithms. It is used by a RegistrationFilter to compute the
 * output displacement field which will map a moving image onto a
 * a fixed image.
 *
 * Non-integer moving image values are obtained by using
 * interpolation. The default interpolator is of type
 * LinearInterpolateImageFunction. The user may set other
 * interpolators via method SetMovingImageInterpolator. Note that the input
 * interpolator must derive from baseclass InterpolateImageFunction.
 *
 * This class is templated over the fixed image type, moving image type,
 * and the displacement field type.
 *
 * \warning This filter assumes that the fixed image type, moving image type
 * and displacement field type all have the same number of dimensions.
 *
 * \sa MIRegistrationFilter
 * \ingroup FiniteDifferenceFunctions
 * \ingroup ITKFEMRegistration
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
class ITK_TEMPLATE_EXPORT MIRegistrationFunction:
  public PDEDeformableRegistrationFunction< TFixedImage,
                                            TMovingImage, TDisplacementField >
{
public:
  /** Standard class typedefs. */
  typedef MIRegistrationFunction Self;
  typedef PDEDeformableRegistrationFunction< TFixedImage,
                                             TMovingImage, TDisplacementField >    Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MIRegistrationFunction,
               PDEDeformableRegistrationFunction);

  /** MovingImage image type. */
  typedef typename Superclass::MovingImageType    MovingImageType;
  typedef typename Superclass::MovingImagePointer MovingImagePointer;

  /** FixedImage image type. */
  typedef typename Superclass::FixedImageType    FixedImageType;
  typedef typename Superclass::FixedImagePointer FixedImagePointer;
  typedef typename FixedImageType::IndexType     IndexType;
  typedef typename FixedImageType::SizeType      SizeType;
  typedef typename FixedImageType::SpacingType   SpacingType;

  /** Displacement field type. */
  typedef typename Superclass::DisplacementFieldType DisplacementFieldType;
  typedef typename Superclass::DisplacementFieldTypePointer
  DisplacementFieldTypePointer;

  /** Inherit some enums from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** Inherit some enums from the superclass. */
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::FloatOffsetType  FloatOffsetType;
  typedef typename Superclass::TimeStepType     TimeStepType;

  /** Interpolator type. */
  typedef double                                                    CoordRepType;
  typedef InterpolateImageFunction< MovingImageType, CoordRepType > InterpolatorType;
  typedef typename InterpolatorType::Pointer                        InterpolatorPointer;
  typedef typename InterpolatorType::PointType                      PointType;
  typedef LinearInterpolateImageFunction< MovingImageType, CoordRepType >
    DefaultInterpolatorType;

  /** Covariant vector type. */
  typedef CovariantVector< double, itkGetStaticConstMacro(ImageDimension) > CovariantVectorType;

  /** Gradient calculator type. */
  typedef CentralDifferenceImageFunction< FixedImageType > GradientCalculatorType;
  typedef typename GradientCalculatorType::Pointer         GradientCalculatorPointer;

  /** Set the moving image interpolator. */
  void SetMovingImageInterpolator(InterpolatorType *ptr)
    { m_MovingImageInterpolator = ptr; }

  /** Get the moving image interpolator. */
  InterpolatorType * GetMovingImageInterpolator(void)
    { return m_MovingImageInterpolator; }

  /** This class uses a constant timestep of 1. */
  TimeStepType ComputeGlobalTimeStep( void *itkNotUsed(GlobalData) ) const override
    { return m_TimeStep; }

  /** Return a pointer to a global data structure that is passed to
   * this object from the solver at each calculation. */
  void * GetGlobalDataPointer() const override
    {
      GlobalDataStruct *global = new GlobalDataStruct();

      return global;
    }

  /** Release memory for global data structure. */
  void ReleaseGlobalDataPointer(void *GlobalData) const override
    { delete (GlobalDataStruct *)GlobalData; }

  /** Set the object's state before each iteration. */
  void InitializeIteration() override;

  /** Compute update at a non boundary neighbourhood.
   * This method is called by a finite difference solver image filter at each
   * pixel that does not lie on a data set boundary. */
  PixelType ComputeUpdate( const NeighborhoodType & neighborhood,
                                    void *globalData,
                                    const FloatOffsetType & offset = FloatOffsetType(0.0) ) override;

  void SetMinNorm( float ts = 1.0 )
    { m_Minnorm = ts; }

  void SetDoInverse( bool b = false )
    { m_DoInverse = b; }

protected:
  MIRegistrationFunction();
  ~MIRegistrationFunction() override {}
  void PrintSelf(std::ostream & os, Indent indent) const override;

  /** FixedImage image neighborhood iterator type. */
  typedef ConstNeighborhoodIterator< FixedImageType > FixedImageNeighborhoodIteratorType;

  /** A global data type for this class of equation. Used to store
   * iterators for the fixed image. */
  struct GlobalDataStruct {
    FixedImageNeighborhoodIteratorType m_FixedImageIterator;
  };

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MIRegistrationFunction);

  /** The global timestep. */
  TimeStepType m_TimeStep;

  SpacingType m_FixedImageSpacing;
  PointType   m_FixedImageOrigin;

  GradientCalculatorPointer m_FixedImageGradientCalculator;
  GradientCalculatorPointer m_MovingImageGradientCalculator;

  InterpolatorPointer m_MovingImageInterpolator;

  /** Threshold below which the denominator term is considered zero. */
  double m_DenominatorThreshold;

  /** Threshold below which two intensity value are assumed to match. */
  double m_IntensityDifferenceThreshold;

  mutable double m_MetricTotal;

  unsigned int m_NumberOfSamples;
  unsigned int m_NumberOfBins;
  float        m_Minnorm;

  bool m_DoInverse;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMIRegistrationFunction.hxx"
#endif

#endif
