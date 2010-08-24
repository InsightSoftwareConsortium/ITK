/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianDerivativeOperator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkGaussianDerivativeOperator_h
#define __itkGaussianDerivativeOperator_h

#include "itkNeighborhoodOperator.h"
#include "itkGaussianOperator.h"
#include "itkDerivativeOperator.h"
#include <math.h>

namespace itk
{
/**
 * \class GaussianDerivativeOperator
 * \brief A NeighborhoodOperator whose coefficients are a one dimensional,
 * discrete derivative Gaussian kernel.
 *
 * GaussianDerivativeOperator can be used to calculate Gaussian derivatives
 * by taking its inner product with to a Neighborhood
 * (NeighborhooIterator) that is swept across an image region.
 * It is a directional operator.  N successive applications
 * oriented along each dimensional direction will calculate separable,
 * efficient, N-D Gaussian derivatives of an image region.
 *
 * GaussianDerivativeOperator takes three parameters:
 *
 * (1) The floating-point variance of the desired Gaussian function.
 *
 * (2) The order of the derivative to be calculated (zero order means
 *     it performs only smoothing as a standard itk::GaussianOperator)
 *
 * (3) The "maximum error" allowed in the discrete Gaussian
 * function.  "Maximum errror" is defined as the difference between the area
 * under the discrete Gaussian curve and the area under the continuous
 * Gaussian. Maximum error affects the Gaussian operator size. Care should
 * be taken not to make this value too small relative to the variance
 * lest the operator size become unreasonably large.
 *
 * References:
 * The Gaussian kernel contained in this operator was described
 * by Tony Lindeberg  (Discrete Scale-Space Theory and the Scale-Space
 * Primal Sketch. Dissertation. Royal Institute of Technology, Stockholm,
 * Sweden. May 1991.).
 *
 * \author Ivan Macia, VICOMTech, Spain, http://www.vicomtech.es
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/1290
 *
 * \sa GaussianOperator
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 * \sa Neighborhood
 *
 * \ingroup Operators
 */
template< class TPixel, unsigned int VDimension = 2,
          class TAllocator = NeighborhoodAllocator< TPixel > >
class ITK_EXPORT GaussianDerivativeOperator:
  public NeighborhoodOperator< TPixel, VDimension, TAllocator >
{
public:
  /** Standard class typedefs. */
  typedef GaussianDerivativeOperator                             Self;
  typedef NeighborhoodOperator< TPixel, VDimension, TAllocator > Superclass;

  /** Neighborhood operator types. */
  typedef GaussianOperator< TPixel, VDimension, TAllocator >   GaussianOperatorType;
  typedef DerivativeOperator< TPixel, VDimension, TAllocator > DerivativeOperatorType;

  /** Constructor. */
  GaussianDerivativeOperator()
  {
    m_Order = 1;
    m_Variance = 1.0;
    m_Spacing = 1.0;
    m_MaximumError = 0.005;
    m_MaximumKernelWidth = 30;
    m_UseDerivativeOperator = false;
    m_NormalizeAcrossScale = true;
  }

  /** Copy constructor */
  GaussianDerivativeOperator(const Self & other):
    NeighborhoodOperator< TPixel, VDimension, TAllocator >(other)
  {
    m_UseDerivativeOperator = other.m_UseDerivativeOperator;
    m_NormalizeAcrossScale = other.m_NormalizeAcrossScale;
    m_Spacing = other.m_Spacing;
    m_Order = other.m_Order;
    m_Variance = other.m_Variance;
    m_MaximumError = other.m_MaximumError;
    m_MaximumKernelWidth = other.m_MaximumKernelWidth;
  }

  /** Assignment operator */
  Self & operator=(const Self & other)
  {
    Superclass::operator=(other);
    m_UseDerivativeOperator = other.m_UseDerivativeOperator;
    m_NormalizeAcrossScale = other.m_NormalizeAcrossScale;
    m_Spacing = other.m_Spacing;
    m_Order = other.m_Order;
    m_Variance = other.m_Variance;
    m_MaximumError = other.m_MaximumError;
    m_MaximumKernelWidth = other.m_MaximumKernelWidth;
    return *this;
  }

  /** Set/Get the flag for choosing the implementation. If we choose
   * to use itk::DerivativeOperator, then the derivative Gaussian kernel
   * is calculated as a convolution with the itk::DerivativeOperator of
   * the desired order. Otherwise a polynomial is computed analitically
   * for the derivative of the Gaussian. */
  void SetUseDerivativeOperator(bool flag)
  {
    if ( m_UseDerivativeOperator != flag )
      {
      m_UseDerivativeOperator = flag;
      }
  }

  bool GetUseDerivativeOperator() const { return m_UseDerivativeOperator; }
  itkBooleanMacro(UseDerivativeOperator);

  /** Set/Get the flag for calculating scale-space normalized derivatives.
    * Normalized derivatives are obtained multiplying by the scale parameter t. */
  void SetNormalizeAcrossScale(bool flag)
  {
    if ( m_NormalizeAcrossScale != flag )
      {
      m_NormalizeAcrossScale = flag;
      }
  }

  bool GetNormalizeAcrossScale() const { return m_NormalizeAcrossScale; }
  itkBooleanMacro(NormalizeAcrossScale);

  /** Sets the desired variance of the Gaussian kernel. */
  void SetVariance(const double variance) { m_Variance = variance; }

  /** Sets the spacing for the direction of this kernel. */
  void SetSpacing(const double spacing)
  {
    m_Spacing = spacing;
  }

  /** Sets the desired maximum error of the gaussian approximation.  Maximum
   * error is the difference between the area under the discrete Gaussian curve
   * and the area under the continuous Gaussian. Maximum error affects the
   * Gaussian operator size. The value is clamped between 0.00001 and 0.99999. */
  void SetMaximumError(const double maxerror)
  {
    const double Min = 0.00001;
    const double Max = 1.0 - Min;

    m_MaximumError = ( maxerror < Min ? Min : ( maxerror > Max ? Max : maxerror ) );
  }

  /** Returns the variance of the Gaussian (scale) for the operator. */
  double GetVariance() { return m_Variance; }

  /** Returns the maximum error of the gaussian approximation.  Maximum error is
   * the difference between the area under the discrete Gaussian curve and the
   * area under the continuous Gaussian. Maximum error affects the Gaussian
   * operator size. */
  double GetMaximumError() { return m_MaximumError; }

  /** Sets a limit for growth of the kernel.  Small maximum error values with
   *  large variances will yield very large kernel sizes.  This value can be
   *  used to truncate a kernel in such instances.  A warning will be given on
   *  truncation of the kernel. */
  void SetMaximumKernelWidth(unsigned int n)
  {
    m_MaximumKernelWidth = n;
  }

  /** Returns the maximum allowed kernel width. */
  unsigned int GetMaximumKernelWidth() const { return m_MaximumKernelWidth; }

  /** Sets the order of the derivative. */
  void SetOrder(const unsigned int order)
  {
    m_Order = order;
  }

  /** Returns the order of the derivative. */
  unsigned int GetOrder() const { return m_Order; }

  /** Prints some debugging information. */
  virtual void PrintSelf(std::ostream & os, Indent i) const
  {
    os << i << "GaussianDerivativeOperator { this=" << this
       << ", m_UseDerivativeOperator = " << m_UseDerivativeOperator
       << ", m_NormalizeAcrossScale = " << m_NormalizeAcrossScale
       << ", m_Order = " << m_Order
       << ", m_Spacing = " << m_Spacing
       << ", m_Variance = " << m_Variance
       << ", m_MaximumError = " << m_MaximumError
       << ", m_MaximumKernelWidth = " << m_MaximumKernelWidth
       << "} "  << std::endl;
    Superclass::PrintSelf( os, i.GetNextIndent() );
  }

protected:

  typedef typename Superclass::CoefficientVector CoefficientVector;

  /** Returns the value of the modified Bessel function I0(x) at a point x >= 0.
    */
  double ModifiedBesselI0(double);

  /** Returns the value of the modified Bessel function I1(x) at a point x,
   * x real.  */
  double ModifiedBesselI1(double);

  /** Returns the value of the modified Bessel function Ik(x) at a point x>=0,
   * where k>=2. */
  double ModifiedBesselI(int, double);

  /** Calculates operator coefficients. */
  CoefficientVector GenerateCoefficients();

  /** Arranges coefficients spatially in the memory buffer. */
  void Fill(const CoefficientVector & coeff)
  { this->FillCenteredDirectional(coeff); }
private:

  /** For compatibility with itkWarningMacro */
  const char * GetNameOfClass()
  { return "itkGaussianDerivativeOperator"; }

  /** Flag to set if the implementation uses the itk::DerivativeOperator. */
  bool m_UseDerivativeOperator;

  /** Normalize derivatives across scale space */
  bool m_NormalizeAcrossScale;

  /** Desired variance of the discrete Gaussian function. */
  double m_Variance;

  /** Difference between the areas under the curves of the continuous and
   * discrete Gaussian functions. */
  double m_MaximumError;

  /** Maximum kernel size allowed.  This value is used to truncate a kernel
   *  that has grown too large.  A warning is given when the specified maximum
   *  error causes the kernel to exceed this size. */
  unsigned int m_MaximumKernelWidth;

  /** Order of the derivative. */
  unsigned int m_Order;

  /** Spacing in the direction of this kernel. */
  double m_Spacing;
};
} // namespace itk

#if 0 //HACK: Not yet implemented
// Define instantiation macro for this template.
#define ITK_TEMPLATE_GaussianDerivativeOperator(_, EXPORT, TypeX, TypeY)     \
  namespace itk                                                              \
  {                                                                          \
  _( 2 ( class EXPORT GaussianDerivativeOperator< ITK_TEMPLATE_2 TypeX > ) ) \
  namespace Templates                                                        \
  {                                                                          \
  typedef GaussianDerivativeOperator< ITK_TEMPLATE_2 TypeX >                 \
  GaussianDerivativeOperator##TypeY;                                       \
  }                                                                          \
  }

#if ITK_TEMPLATE_EXPLICIT
#include "Templates/itkGaussianDerivativeOperator+-.h"
#endif
#endif

#if ITK_TEMPLATE_TXX
#include "itkGaussianDerivativeOperator.txx"
#endif

#endif
