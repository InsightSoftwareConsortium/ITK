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
#ifndef itkGaussianDerivativeOperator_h
#define itkGaussianDerivativeOperator_h

#include "itkGaussianOperator.h"
#include "itkDerivativeOperator.h"

#include <algorithm>

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
 * This implementation is derived from the Insight Journal paper:
 * https://hdl.handle.net/1926/1290
 *
 * \sa GaussianOperator
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 * \sa Neighborhood
 *
 * \ingroup Operators
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{Operators/GaussianDerivativeOperator,Create a Gaussian derivative kernel}
 * \endwiki
 */
template< typename TPixel, unsigned int VDimension = 2,
          typename TAllocator = NeighborhoodAllocator< TPixel > >
class ITK_TEMPLATE_EXPORT GaussianDerivativeOperator :
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
  GaussianDerivativeOperator();

  /** Copy constructor */
  GaussianDerivativeOperator(const Self & other);

  /** Assignment operator */
  Self & operator=(const Self & other);


  /** Set/Get the flag for calculating scale-space normalized
   * derivatives.
   *
   * Normalized derivatives are obtained multiplying by the scale
   * parameter $t^1/order$. This use useful for scale-space selection
   * algorithms such as blob detection. The scaling results in the
   * value of the derivatives being independent of the size of an
   * object. */
  void SetNormalizeAcrossScale(bool flag) { m_NormalizeAcrossScale = flag; }
  bool GetNormalizeAcrossScale() const { return m_NormalizeAcrossScale; }
  itkBooleanMacro(NormalizeAcrossScale);

  /** Set/Get the variance of the Gaussian kernel.
   *
   */
  void SetVariance(const double variance) { m_Variance = variance; }
  double GetVariance() const { return m_Variance; }

  /** Set/Get the spacing for the direction of this kernel. */
  void SetSpacing(const double spacing) { m_Spacing = spacing; }
  double GetSpacing() const { return m_Spacing; }

  /** Set/Get the desired maximum error of the gaussian approximation.  Maximum
   * error is the difference between the area under the discrete Gaussian curve
   * and the area under the continuous Gaussian. Maximum error affects the
   * Gaussian operator size. The value is clamped between 0.00001 and 0.99999. */
  void SetMaximumError(const double maxerror)
  {
    const double Min = 0.00001;
    const double Max = 1.0 - Min;

    m_MaximumError = std::max( Min, std::min( Max, maxerror ) );
  }
  double GetMaximumError() { return m_MaximumError; }

  /** Sets/Get a limit for growth of the kernel.  Small maximum error values with
   *  large variances will yield very large kernel sizes.  This value can be
   *  used to truncate a kernel in such instances.  A warning will be given on
   *  truncation of the kernel. */
  void SetMaximumKernelWidth(unsigned int n)
  {
    m_MaximumKernelWidth = n;
  }

  /** Sets/Get the order of the derivative. */
  void SetOrder(const unsigned int order) { m_Order = order;}
  unsigned int GetOrder() const { return m_Order; }

  /** Prints member variables */
  virtual void PrintSelf(std::ostream & os, Indent i) const;

protected:

  typedef typename Superclass::CoefficientVector CoefficientVector;

  /** Returns the value of the modified Bessel function I0(x) at a point x >= 0.
    */
  static double ModifiedBesselI0(double);

  /** Returns the value of the modified Bessel function I1(x) at a point x,
   * x real.  */
  static double ModifiedBesselI1(double);

  /** Returns the value of the modified Bessel function Ik(x) at a point x>=0,
   * where k>=2. */
  static double ModifiedBesselI(int, double);

  /** Calculates operator coefficients. */
  CoefficientVector GenerateCoefficients();

  /** Arranges coefficients spatially in the memory buffer. */
  void Fill(const CoefficientVector & coeff)
  { this->FillCenteredDirectional(coeff); }

private:

  /* methods for generations of the coeeficients for a gaussian
   * operator of 0-order respecting the remaining parameters */
  CoefficientVector GenerateGaussianCoefficients() const;

  /** For compatibility with itkWarningMacro */
  const char * GetNameOfClass() const
  {
    return "itkGaussianDerivativeOperator";
  }

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

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaussianDerivativeOperator.hxx"
#endif

#endif
