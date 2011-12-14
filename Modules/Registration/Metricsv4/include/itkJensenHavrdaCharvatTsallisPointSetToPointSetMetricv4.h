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
#ifndef __itkJensenHavrdaCharvatTsallisPointSetToPointSetMetricv4_h
#define __itkJensenHavrdaCharvatTsallisPointSetToPointSetMetricv4_h

#include "itkPointSetToPointSetMetricv4.h"

#include "itkManifoldParzenWindowsPointSetFunction.h"

namespace itk {

/** \class JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4
 *
 * \brief Implementation of the Jensen Havrda Charvat Tsallis Point Set metric.
 *
 * Given a specified transform and direction, this class calculates the
 * value and derivative between a "fixed" and "moving" point set pair using
 * the Havrda-Charvat-Tsallis entropy family, a generalization of the well-known
 * Shannon entropy, and the Jensen divergence.  Another way to look at the
 * family of information-theoretic measures is that the points are used to
 * construct the corresponding probably density functions.
 *
 * In addition, we allow the user to invoke a manifold parzen windowing of the
 * data.  Instead of an isotropic Gaussian being associated with each point,
 * we can actually calculate the covariance matrix for each point such that it
 * reflects the locate point set structure.
 *
 * To speed up the metric calculation, we use ITK's K-d tree to query the
 * metric value only for a given neighborhood.  Considering that probably only
 * a small subset of points is needed to get a good approximation of the
 * metric value for a single point, this is probably warranted.  So what we do
 * is transform each point (with the specified transform) and construct the
 * k-d tree from the transformed points.
 *
 * Contributed by Nicholas J. Tustison, James C. Gee in the Insight Journal
 * paper:  http://hdl.handle.net/1926/1524
 *
 * \par REFERENCE
 *
 * N.J. Tustison, S. P. Awate, G. Song, T. S. Cook, and J. C. Gee.
 * "Point set registration using Havrda-Charvat-Tsallis entropy measures"
 * IEEE Transactions on Medical Imaging, 30(2):451-60, 2011.
 * \ingroup ITKMetricsv4
 */

template<class TPointSet>
class ITK_EXPORT JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4 :
    public PointSetToPointSetMetricv4<TPointSet, TPointSet>
{
public:
  /** Standard class typedefs. */
  typedef JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4 Self;
  typedef PointSetToPointSetMetricv4<TPointSet, TPointSet>     Superclass;
  typedef SmartPointer<Self>                                   Pointer;
  typedef SmartPointer<const Self>                             ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods) */
  itkTypeMacro( JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4, PointSetToPointSetMetricv4 );

  typedef TPointSet                                 PointSetType;
  typedef typename PointSetType::PointsContainer    PointsContainer;
  typedef typename PointsContainer::ConstIterator   PointsContainerConstIterator;

  itkStaticConstMacro( PointDimension, unsigned int, TPointSet::PointDimension );

  /** Types transferred from the base class */
  typedef typename Superclass::MeasureType              MeasureType;
  typedef typename Superclass::DerivativeType           DerivativeType;
  typedef typename Superclass::LocalDerivativeType      LocalDerivativeType;
  typedef typename Superclass::PointType                PointType;
  typedef typename Superclass::CoordRepType             CoordRepType;
  typedef typename Superclass::PointIdentifier          PointIdentifier;
  typedef typename Superclass::NeighborsIdentifierType  NeighborsIdentifierType;

  typedef MeasureType                                   RealType;

  /**
   * Other typedefs
   */
  typedef ManifoldParzenWindowsPointSetFunction
    <PointSetType, RealType>                            DensityFunctionType;
  typedef typename DensityFunctionType::GaussianType    GaussianType;
  typedef typename DensityFunctionType::Pointer         DensityFunctionPointer;

  /** Initialize the Metric by making sure that all the components
   *  are present and plugged together correctly     */
  virtual void Initialize( void ) throw ( ExceptionObject );

  /**
   * This method returns the value of the metric based on the current
   * transformation(s).
   */
  virtual MeasureType GetValue() const;

  /**
   * This method returns the derivative based on the current
   * transformation(s).
   */
  virtual void GetDerivative( DerivativeType & ) const;

  /**
   * This method returns the derivative and value based on the current
   * transformation(s).
   */
  virtual void GetValueAndDerivative( MeasureType &, DerivativeType & ) const;

  /**
   * Set the alpha parameter used to tune the point-set metric from
   * a maximum-likelihood measure (alpha = 1) to the more robust L2
   * solution (alpha = 2).  Typically, "robustness" is associated with
   * performance in the presence of uniform noise but in most applications
   * the noise will be highly correlated with the point sets, therefore
   * an alpha value close to 1, in general, provides better performance.
   * Only values between 1 and 2 are convex.
   */
  itkSetClampMacro( Alpha, RealType, 1.0, 2.0 );

  /**
   * Get the alpha parameter used to tune the point-set metric.
   */
  itkGetConstMacro( Alpha, RealType );

  /**
   * The Jensen divergence is comprised of two terms---one measures the
   * mutual similarity while the second provides a regularizing effect
   * penalizing tendencies towards a single point.  For most registration
   * applications, a separate transform would be used which would provide
   * its own regularization.  Default = false.
   */
  itkSetMacro( UseRegularizationTerm, bool );

  /** Get boolean usage of regularization term. */
  itkGetConstMacro( UseRegularizationTerm, bool );

  /** Get/set boolean usage of regularization term. */
  itkBooleanMacro( UseRegularizationTerm );

  /**
   * Each point is associated with a Gaussian characterized by m_PointSetSigma
   * which provides a sense of scale for determining the similarity between two
   * point sets.  Default = 1.0.
   */
  itkSetMacro( PointSetSigma, RealType );

  /** Get the point set sigma function */
  itkGetConstMacro( PointSetSigma, RealType );

  /**
   * Set the neighborhood size used to evaluate the measurement at each
   * point.  Default = 50.
   */
  itkSetMacro( EvaluationKNeighborhood, unsigned int );

  /**
   * Get the neighborhood size used to evaluate the measurement at each
   * point.  Default = 50.
   */
  itkGetConstMacro( EvaluationKNeighborhood, unsigned int );

  /**
   * Set whether or not anisotropic covariances are determined for each
   * Gaussian.  Default = false.
   */
  itkSetMacro( UseAnisotropicCovariances, bool );

  /**
   * Get whether or not anisotropic covariances are determined for each
   * Gaussian.  Default = false.
   */
  itkGetConstMacro( UseAnisotropicCovariances, bool );

  /**
   * Get/set whether or not anisotropic covariances are determined for each
   * Gaussian.  Default = false.
   */
  itkBooleanMacro( UseAnisotropicCovariances );

  /**
   * Set the size of the covariance neighborhood used to construct the
   * anisotropic covariances.  Only relevant if m_UseAnisotropicCovariances =
   * true.  Default = 5.
   */
  itkSetMacro( CovarianceKNeighborhood, unsigned int );

  /**
   * Get the size of the covariance neighborhood used to construct the
   * anisotropic covariances.  Only relevant if m_UseAnisotropicCovariances =
   * true.  Default = 5.
   */
  itkGetConstMacro( CovarianceKNeighborhood, unsigned int );

  /**
   * Set the size of the noise kernel used to construct each covariance image.
   * To avoid the case where the local point set structure would result in a
   * degenerate covariance matrix, a small amount of noise is added along the
   * diagonal represented by this variable.  Only relevant if
   * m_UseAnisotropicCovariances = true.  Default = 10.0.
   */
  itkSetMacro( KernelSigma, RealType );

  /** Get the noise kernel sigma for the anistropic covariances. */
  itkGetConstMacro( KernelSigma, RealType );

  /** Pure virtual function from the parent class not needed in this class */
  virtual MeasureType GetLocalNeighborhoodValue( const PointType & ) const
  {
    itkExceptionMacro( "This function should not be accessed." );
    return 0;
  }

  /** Pure virtual function from the parent class not needed in this class */
  virtual void GetLocalNeighborhoodValueAndDerivative( const PointType &,
    MeasureType &, LocalDerivativeType & ) const
  {
    itkExceptionMacro( "This function should not be accessed." );
  }

protected:
  JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4();
  ~JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4();

  void PrintSelf( std::ostream& os, Indent indent ) const;

private:
  //purposely not implemented
  JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4( const Self& );
  void operator=( const Self& );

  DensityFunctionPointer                   m_MovingDensityFunction;
  DensityFunctionPointer                   m_FixedDensityFunction;

  bool                                     m_UseRegularizationTerm;
  bool                                     m_UseAnisotropicCovariances;

  RealType                                 m_PointSetSigma;
  RealType                                 m_KernelSigma;
  unsigned int                             m_CovarianceKNeighborhood;
  unsigned int                             m_EvaluationKNeighborhood;

  RealType                                 m_Alpha;
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkJensenHavrdaCharvatTsallisPointSetToPointSetMetricv4.hxx"
#endif

#endif
