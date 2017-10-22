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
#ifndef itkTimeVaryingVelocityFieldIntegrationImageFilter_h
#define itkTimeVaryingVelocityFieldIntegrationImageFilter_h

#include "itkImageToImageFilter.h"

#include "itkVectorInterpolateImageFunction.h"

namespace itk
{
/**
 * \class TimeVaryingVelocityFieldIntegrationImageFilter
 * \brief Integrate a time-varying velocity field using 4th order Runge-Kutta.
 *
 * Diffeomorphisms are topology-preserving mappings that are useful for
 * describing biologically plausible deformations.  Mathematically, a
 * diffeomorphism, \f$ \phi \f$, is generated from a time-varying velocity field, v, as
 * described by the integral equation:
 *
 * \f[
 * \phi(t_b) = \phi(t_a) + \int_{t_a}^{t_b} v(\phi(t),t) dt
 * \f]
 *
 * In this class, the input is the time-varying velocity field and an initial
 * diffeomorophism.  The output diffeomorphism is produced using fourth order
 * Runge-Kutta.
 *
 * \warning The output deformation field needs to have dimensionality of 1
 * less than the input time-varying velocity field.
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKDisplacementField
 */
template<typename TTimeVaryingVelocityField, typename TDisplacementField =
 Image<typename TTimeVaryingVelocityField::PixelType,
 TTimeVaryingVelocityField::ImageDimension - 1> >
class ITK_TEMPLATE_EXPORT TimeVaryingVelocityFieldIntegrationImageFilter :
  public ImageToImageFilter<TTimeVaryingVelocityField, TDisplacementField>
{
public:
  typedef TimeVaryingVelocityFieldIntegrationImageFilter  Self;
  typedef ImageToImageFilter
    <TTimeVaryingVelocityField, TDisplacementField>       Superclass;
  typedef SmartPointer<Self>                              Pointer;
  typedef SmartPointer<const Self>                        ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Run-time type information ( and related methods ) */
  itkTypeMacro( TimeVaryingVelocityFieldIntegrationImageFilter, ImageToImageFilter );

  /**
   * Dimensionality of input data is assumed to be one more than the output
   * data the same. */
  itkStaticConstMacro( InputImageDimension, unsigned int,
    TTimeVaryingVelocityField::ImageDimension );

  itkStaticConstMacro( OutputImageDimension, unsigned int,
    TDisplacementField::ImageDimension );

  typedef TTimeVaryingVelocityField                   TimeVaryingVelocityFieldType;
  typedef TDisplacementField                          DisplacementFieldType;
  typedef typename DisplacementFieldType::Pointer     DisplacementFieldPointer;
  typedef typename DisplacementFieldType::PixelType   VectorType;
  typedef typename VectorType::RealValueType          RealType;
  typedef typename VectorType::ValueType              ScalarType;
  typedef typename DisplacementFieldType::PointType   PointType;
  typedef typename DisplacementFieldType::RegionType  OutputRegionType;

  typedef VectorInterpolateImageFunction
    <TimeVaryingVelocityFieldType, ScalarType>    VelocityFieldInterpolatorType;
  typedef typename VelocityFieldInterpolatorType::Pointer
                                                  VelocityFieldInterpolatorPointer;

  typedef VectorInterpolateImageFunction<DisplacementFieldType, ScalarType>   DisplacementFieldInterpolatorType;
  typedef typename DisplacementFieldInterpolatorType::Pointer                 DisplacementFieldInterpolatorPointer;

  /** Get/Set the time-varying velocity field interpolator.  Default = linear. */
  itkSetObjectMacro( VelocityFieldInterpolator, VelocityFieldInterpolatorType );
  itkGetModifiableObjectMacro(VelocityFieldInterpolator, VelocityFieldInterpolatorType );

  /**
   * Get/Set the deformation field interpolator for the initial diffeomorphism
   * (if set).  Default = linear.
   */
  itkSetObjectMacro( DisplacementFieldInterpolator, DisplacementFieldInterpolatorType );
  itkGetModifiableObjectMacro(DisplacementFieldInterpolator, DisplacementFieldInterpolatorType );

  /**
   * Get/Set the initial diffeomorphism
   */
  itkSetObjectMacro( InitialDiffeomorphism, DisplacementFieldType );
  itkGetModifiableObjectMacro(InitialDiffeomorphism, DisplacementFieldType );

  /**
   * Set the lower time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1].
   */
  itkSetClampMacro( LowerTimeBound, RealType, 0, 1 );

  /**
   * Get the lower time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1].
   */
  itkGetConstMacro( LowerTimeBound, RealType );

  /**
   * Set the upper time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1].
   */
  itkSetClampMacro( UpperTimeBound, RealType, 0, 1 );

  /**
   * Get the upper time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1].
   */
  itkGetConstMacro( UpperTimeBound, RealType );

  /**
   * Set the number of integration steps used in the Runge-Kutta solution of the
   * initial value problem.  Default = 10.
   */
  itkSetMacro( NumberOfIntegrationSteps, unsigned int );

  /**
   * Get the number of integration steps used in the Runge-Kutta solution of the
   * initial value problem.  Default = 10.
   */
  itkGetConstMacro( NumberOfIntegrationSteps, unsigned int );

protected:
  TimeVaryingVelocityFieldIntegrationImageFilter();
  ~TimeVaryingVelocityFieldIntegrationImageFilter() ITK_OVERRIDE;

  void PrintSelf( std::ostream & os, Indent indent ) const ITK_OVERRIDE;

  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  virtual void BeforeThreadedGenerateData() ITK_OVERRIDE;

  virtual void ThreadedGenerateData( const OutputRegionType &, ThreadIdType ) ITK_OVERRIDE;

  VectorType IntegrateVelocityAtPoint( const PointType &initialSpatialPoint, const TimeVaryingVelocityFieldType * inputField );

  RealType                                  m_LowerTimeBound;
  RealType                                  m_UpperTimeBound;

  DisplacementFieldPointer                  m_InitialDiffeomorphism;

  unsigned int                              m_NumberOfIntegrationSteps;

  unsigned int                              m_NumberOfTimePoints;

  DisplacementFieldInterpolatorPointer      m_DisplacementFieldInterpolator;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TimeVaryingVelocityFieldIntegrationImageFilter);

  VelocityFieldInterpolatorPointer          m_VelocityFieldInterpolator;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTimeVaryingVelocityFieldIntegrationImageFilter.hxx"
#endif

#endif
