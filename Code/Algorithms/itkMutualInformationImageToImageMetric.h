/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMutualInformationImageToImageMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkMutualInformationImageToImageMetric_h
#define __itkMutualInformationImageToImageMetric_h

#include "itkSimilarityRegistrationMetric.h"
#include "itkCovariantVector.h"
#include "itkPoint.h"

#include "itkIndex.h"
#include "itkKernelFunction.h"
#include "itkCentralDifferenceImageFunction.h"
#include "itkImageMapper.h"

namespace itk
{

/** \class MutualInformationImageToImageMetric
 * \brief Computes the mutual information between two images to be registered
 *
 * MutualInformationImageToImageMetric computes the mutual information
 * between a target and reference image to be registered.
 *
 * This class is templated over two types:
 *    TTarget = the target image type,
 *    TMapper = the mapper type,
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
class TMapper >
class ITK_EXPORT MutualInformationImageToImageMetric :
  public SimilarityRegistrationMetric< 
    TTarget, TMapper, double,
    CovariantVector< double, TMapper::SpaceDimension > >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef MutualInformationImageToImageMetric  Self;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Space dimension is the dimension of parameters space
   */
   enum { SpaceDimension = TMapper::SpaceDimension };

  /**
   *  Type of the match measure
   */
  typedef double  MeasureType;

  /**
   *  Type of the derivative of the match measure
   */
  typedef CovariantVector<MeasureType,SpaceDimension>  DerivativeType;

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
   * Standard "Superclass" typedef.
   */
  typedef SimilarityRegistrationMetric< 
      TargetType, MapperType, 
      MeasureType, DerivativeType >  Superclass;

  /**
   * TargetImageDimension enumeration
   */
  enum { TargetImageDimension = TargetType::ImageDimension };

  /**
   * Type of the Transform
   */
  typedef typename MapperType::TransformationType TransformationType;

  /**
   *  Parameters type
   */
  typedef typename MapperType::ParametersType ParametersType;

  /**
   *  Pointer type for the Reference
   */
  typedef typename ReferenceType::ConstPointer ReferenceConstPointer;

  /**
   *  Pointer type for the Target
   */
  typedef typename TargetType::ConstPointer TargetConstPointer;

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
  typedef typename MapperType::OutputPointType TargetPointType;

  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(MutualInformationImageToImageMetric, SimilarityRegistrationMetric);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Get the Derivatives of the Match Measure
   */
  const DerivativeType& GetDerivative( const ParametersType& parameters );

  /**
   *  Get the Value
   */
  MeasureType GetValue( const ParametersType& parameters );

  /**
   *  Get Value and Derivatives for SingleValuedOptimizers
   */
  void GetValueAndDerivative( const ParametersType& parameters, 
    MeasureType& Value, DerivativeType& Derivative );

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
        return *this;
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
   * DerivativeContainer typedef support
   */
  typedef std::vector<DerivativeType> DerivativeContainer;

  /**
   * Container to store sample set A image derivatives
   */
  DerivativeContainer                 m_SampleADerivatives;

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
  // This solution only works with a transform that has a
  // GetJacobian() API.
  //----------------------------------------------------------
  /**
   * Calculate the intensity derivatives at a point
   */
  void CalculateDerivatives(TargetPointType& , DerivativeType& );

  typedef CentralDifferenceImageFunction< ReferenceType >
    DerivativeFunctionType;

  typename DerivativeFunctionType::Pointer  m_DerivativeCalculator;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMutualInformationImageToImageMetric.txx"
#endif

#endif

