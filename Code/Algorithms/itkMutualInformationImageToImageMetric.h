/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMutualInformationImageToImageMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkMutualInformationImageToImageMetric_h
#define __itkMutualInformationImageToImageMetric_h

#include "itkObject.h"
#include "itkVectorContainer.h"
#include "itkIndex.h"
#include "itkKernelFunction.h"
#include "itkCentralDerivativeImageFunction.h"
#include "itkImageMapper.h"

namespace itk
{

/** \class MutualInformationImageToImageMetric
 * \brief Computes the mutual information between two images to be registered
 *
 * MutualInformationImageToImageMetric computes the mutual information
 * between a target and reference image to be registered.
 *
 * This class is templated over four types:
 *    TTarget = the target image type,
 *    TMapper = the mapper type,
 *    TMeasure = type of the output metric value, and
 *    TDerivative = type of each of the individual metric derivatives
 *
 * For a given set of transform parameters, the mapper calculates the
 * transformed reference image value at a target domain point.
 * The transform parameters can be defined via SetParameters().
 *
 * The methods SetTarget() and SetMapper() are used to define the
 * the target image and the mapper. Note that the reference image has
 * to connected to the mapper (outside of this class) before the
 * mutual information value can be calculated.
 *
 * The method GetValue() evokes the calculation of the mutual information
 * while method GetValueAndDerivative() evokes the calculation of
 * both the mutual information and its derivatives with respect to the
 * transform parameters.
 *
 * The calculations are based on the method of Viola and Wells
 * where the probability density distributions are estimated using
 * Parzen windows.
 *
 * By default a Gaussian kernel is used in the density estimation.
 * Other option include Cauchy and spline-based. A user can specify
 * the kernel passing in a pointer a KernelFunction using the
 * SetKernelFunction() method.
 *
 * Mutual information is estimated using two sample sets: one to calculate
 * the singular and joint pdf's and one to calculate the entropy
 * integral. By default 50 samples points are used in each set.
 * Other values can be set via the SetNumberOfSpatialSamples() method.
 *
 * Quality of the density estimate depends on the choice of the
 * kernel's variance. Optimal choice will depend on the images.
 * It is can be shown that around the optimal variance, the mutual
 * information estimate is relatively insensitive to small changes
 * of the variance. In our experiments, we have found that a
 * variance of 0.1 works well for images normalized between 0 and 1.
 * The variance can be set via methods SetTargetStandardDeviation()
 * and SetReferenceStandardDeviation().
 *
 * Implementaton of this class is based on:
 * Viola, P. and Wells III, W. (1997).
 * "Alignment by Maximization of Mutual Information"
 * International Journal of Computer Vision, 24(2):137-154
 *
 * Caveat:
 * Calculating the mutual information works for all transform type.
 * However, in order to calculate the derivatives, the mapper has to
 * have the ability to provide derivatives of the reference intensity
 * with respect to the transform parameters.
 *
 * This feature is still to be implemented.
 *
 * A temporary solution for has been implemented in this class.
 * It should be removed once the feature has been implemented in
 * the mapper.
 *
 */
template <
class TTarget,
class TMapper,
class TMeasure,
class TDerivative >
class ITK_EXPORT MutualInformationImageToImageMetric : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef MutualInformationImageToImageMetric  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   *  Type of the Mapper
   */
  typedef TMapper MapperType;

  /**
   *  Type of the Reference
   */
  typedef typename MapperType::DomainType  ReferenceType;

  /**
   *  Type of the Target
   */
  typedef TTarget TargetType;

  /**
   * TargetImageDimension enumeration
   */
  enum { TargetImageDimension = TargetType::ImageDimension };

  /**
   *  Type of the match measure
   */
  typedef TMeasure  MeasureType;

  /**
   * Type of the Transform
   */
  typedef typename MapperType::TransformationType TransformationType;

  /**
   * Space dimension is the dimension of parameters space
   */
   enum { SpaceDimension = TMapper::SpaceDimension };

  /**
   *  Parameters type
   */
  typedef typename TransformationType::ParametersType ParametersType;

  /**
   *  Type of the derivative of the match measure
   */
  typedef Vector<TDerivative,SpaceDimension>  DerivativeType;

  /**
   * Type of the vector match measure
   */
  typedef Vector<TMeasure,SpaceDimension>  VectorMeasureType;

  /**
   *  Pointer type for the Reference
   */
  typedef typename ReferenceType::Pointer ReferencePointer;

  /**
   *  Pointer type for the Target
   */
  typedef typename TargetType::Pointer TargetPointer;

  /**
   *  Pointer type for the Mapper
   */
  typedef typename MapperType::Pointer MapperPointer;

  /**
   * TargetIndex typedef support
   */
  typedef Index<TargetImageDimension> TargetIndexType;

  /**
   * TargetPoint typedef support
   */
  typedef typename MapperType::PointType TargetPointType;

  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(MutualInformationImageToImageMetric, Object);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Connect the Target
   */
  void SetTarget( TargetType * );

  /**
   * Connect the Mapper
   */
  void SetMapper( MapperType * );

  /**
   * Set the Transformation parameters
   */
  void SetParameters( const ParametersType& params )
    { m_Parameters = params; }

  /**
   * Get Parameters
   */
  const ParametersType& GetParameters( void ) const
    { return m_Parameters; }

  /**
   * Get the Derivatives of the Match Measure
   */
  DerivativeType& GetDerivative( void );

  /**
   *  Get the Value for SingleValue Optimizers
   */
  MeasureType GetValue( void );

  /**
   *  Get the Value for MultipleValuedOptimizers
   */
  void GetValue( VectorMeasureType& );

  /**
   *  Get Value and Derivatives for SingleValuedOptimizers
   */
  void GetValueAndDerivative(MeasureType& Value, DerivativeType& Derivative );

  /**
   * Set the number of spatial samples. This is the number of image
   * samples used to calculate the joint probability distribution.
   * Default value is 50.
   */
  void SetNumberOfSpatialSamples( unsigned int num )
    {
    m_NumberOfSpatialSamples = num;
    m_SampleA.resize( m_NumberOfSpatialSamples );
    m_SampleB.resize( m_NumberOfSpatialSamples );
    }

  /**
   * Get the number of spatial samples.
   */
  itkGetConstMacro( NumberOfSpatialSamples, unsigned int );

  /**
   * Set the reference image intensitiy standard deviation. This
   * defines the kernel bandwidth used in the joint probability
   * distribution calculation. Default value is 0.1 which works
   * well for image intensities normalized to between 0 and 1.
   */
  itkSetClampMacro( ReferenceStandardDeviation, double, 0.0,
    NumericTraits<double>::max() );

  /**
   * Get the reference image intensity standard deviation.
   */
  itkGetConstMacro( ReferenceStandardDeviation, double );

  /**
   * Set the target image intensitiy standard deviation. This defines
   * the kernel bandwidth used in the joint probability distribution
   * calculation. Default value is 0.1 which works well for image
   * intensities normalized to between 0 and 1.
   */
  itkSetClampMacro( TargetStandardDeviation, double, 0.0,
    NumericTraits<double>::max() );

  /**
   * Get the target image intensity standard deviation.
   */
  itkGetMacro( TargetStandardDeviation, double );

  /**
   * Set the kernel function. This is used to calculate the joint
   * probability distribution. Default is the GaussianKernelFunction.
   */
  void SetKernelFunction( KernelFunction * ptr )
    { m_KernelFunction = ptr; }

  /**
   * Get the kernel function.
   */
  KernelFunction * GetKernelFunction( void )
    { return m_KernelFunction; }

protected:

  ReferencePointer            m_Reference;
  TargetPointer               m_Target;
  MapperPointer               m_Mapper;
  MeasureType                 m_MatchMeasure;
  VectorMeasureType           m_VectorMatchMeasure;
  DerivativeType              m_MatchMeasureDerivatives;
  ParametersType              m_Parameters;

  MutualInformationImageToImageMetric();
  virtual ~MutualInformationImageToImageMetric() {};
  MutualInformationImageToImageMetric(const Self&) {}
  void operator=(const Self&) {}

private:

  /**
   * A spatial sample consists of
   *   the target domain point,
   *   the target value and that point, and
   *   the corresponding reference value
   */
  class SpatialSample
  {
  public:

    SpatialSample(){};
    ~SpatialSample(){};

    SpatialSample& operator= ( const SpatialSample& rhs )
      {
        if( this == &rhs ) return *this;
        TargetPointValue = rhs.TargetPointValue;
        TargetValue = rhs.TargetValue;
        ReferenceValue = rhs.ReferenceValue;
      }

    TargetPointType                  TargetPointValue;
    double                           TargetValue;
    double                           ReferenceValue;
  };

  /**
   * SpatialSampleContainer typedef support
   */
  typedef std::vector<SpatialSample>  SpatialSampleContainer;

  /**
   * Container to store sample set  A - used to approximate the probability
   * density function (pdf)
   */
  SpatialSampleContainer              m_SampleA;

  /**
   * Container to store sample set  B - used to approximate the mutual
   * information value
   */
  SpatialSampleContainer              m_SampleB;

  /**
   * IntensityDerivative typedef support
   */
  typedef Vector<double, SpaceDimension> IntensityDerivativeType;

  /**
   * IntensityDerivativeContainer typedef support
   */
  typedef std::vector<IntensityDerivativeType> IntensityDerivativeContainer;

  /**
   * Container to store sample set A image derivatives
   */
  IntensityDerivativeContainer      m_SampleADerivatives;

  unsigned int                        m_NumberOfSpatialSamples;
  double                              m_ReferenceStandardDeviation;
  double                              m_TargetStandardDeviation;
  typename KernelFunction::Pointer    m_KernelFunction;

  /**
   * Uniformly select samples from the target image buffer
   */
  void SampleTargetDomain( SpatialSampleContainer& samples );

  //-----------------------------------------------------------
  // The following methods and variables are related to
  // calculating the reference image intensity derivatives
  // with respect to the transform parameters.
  //
  // This should really be done by the mapper.
  //
  // This is a temporary solution until it has been
  // implementation in the mappper.
  // This solution only works any transform that has a
  // GetJacobian() API.
  //----------------------------------------------------------
  /**
   * Calculate the intensity derivatives at a point
   */
  void CalculateDerivatives(TargetPointType& , IntensityDerivativeType& );

  typedef CentralDerivativeImageFunction< ReferenceType >
    DerivativeFunctionType;

  typename DerivativeFunctionType::Pointer  m_DerivativeCalculator;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMutualInformationImageToImageMetric.txx"
#endif

#endif

