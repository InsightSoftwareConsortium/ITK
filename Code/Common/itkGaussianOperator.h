/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianOperator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGaussianOperator_h
#define __itkGaussianOperator_h

#include "itkNeighborhoodOperator.h"
#include <math.h>
namespace itk {

/**
 * \class GaussianOperator
 * \brief A NeighborhoodOperator whose coefficients are a one
 * dimensional, discrete Gaussian kernel.
 *
 * GaussianOperator can be used to perform Gaussian blurring
 * by taking its inner product with to a Neighborhood
 * (NeighborhooIterator) that is swept across an image region.
 * It is a directional operator.  N successive applications
 * oriented along each dimensional direction will effect separable,
 * efficient, N-D Gaussian blurring of an image region.
 *
 * GaussianOperator takes two parameters:
 
 * (1) The floating-point variance of the desired Gaussian function.
 
 * (2) The "maximum error" allowed in the discrete Gaussian
 * function.  "Maximum errror" is defined as the difference between the area
 * under the discrete Gaussian curve and the area under the continuous
 * Gaussian. Maximum error affects the Gaussian operator size. Care should
 * be taken not to make this value too small relative to the variance
 * lest the operator size become unreasonably large.
 *
 * References:
 * The Gaussian kernel contained in this operator was described
 * by Tony Lindeberg (Discrete Scale-Space Theory and the Scale-Space
 * Primal Sketch.  Dissertation. Royal Institute of Technology, Stockholm,
 * Sweden. May 1991.).
 *
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 * \sa Neighborhood
 *
 * \ingroup Operators
 */
template<class TPixel,unsigned int VDimension=2,
  class TAllocator = NeighborhoodAllocator<TPixel> >
class ITK_EXPORT GaussianOperator
  : public NeighborhoodOperator<TPixel, VDimension, TAllocator>
{
public:
  /** Standard class typedefs. */
  typedef GaussianOperator Self;
  typedef NeighborhoodOperator<TPixel, VDimension, TAllocator>  Superclass;
  
  /** Constructor. */
  GaussianOperator() : m_Variance(1), m_MaximumError(.01), m_MaximumKernelWidth(30) { }

  /** Copy constructor */
  GaussianOperator(const Self &other)
    : NeighborhoodOperator<TPixel, VDimension, TAllocator>(other)
  {
    m_Variance = other.m_Variance;
    m_MaximumError = other.m_MaximumError;
  }

  /** Assignment operator */
  Self &operator=(const Self &other)
  {
    Superclass::operator=(other);
    m_Variance = other.m_Variance;
    m_MaximumError = other.m_MaximumError;
    m_MaximumKernelWidth = other.m_MaximumKernelWidth;
    return *this;
  }
  
  /** Sets the desired variance of the Gaussian kernel. */
  void SetVariance(const double &variance)
  {  m_Variance = variance;  }

  /** Sets the desired maximum error of the gaussian approximation.  Maximum
   * error is the difference between the area under the discrete Gaussian curve
   * and the area under the continuous Gaussian. Maximum error affects the
   * Gaussian operator size. */
  void SetMaximumError(const double &max_error)
  {
    if (max_error >= 1 || max_error <= 0)
      {
        throw ExceptionObject(__FILE__, __LINE__);
      }
    
    m_MaximumError = max_error;
  }
  
  /** Returns the variance of the Gaussian (scale) for the operator. */
  double GetVariance()
    {  return m_Variance;  }

  /** Returns the maximum error of the gaussian approximation.  Maximum error is 
   * the difference between the area under the discrete Gaussian curve and the
   * area under the continuous Gaussian. Maximum error affects the Gaussian
   * operator size. */
  double GetMaximumError()
    {    return m_MaximumError;  }

  /** Sets a limit for growth of the kernel.  Small maximum error values with
   *  large variances will yield very large kernel sizes.  This value can be
   *  used to truncate a kernel in such instances.  A warning will be given on
   *  truncation of the kernel. */
  void SetMaximumKernelWidth( unsigned int n )
    {    m_MaximumKernelWidth = n; }

  /** Returns the maximum allowed kernel width. */
  unsigned int GetMaximumKernelWidth() const
    {   return m_MaximumKernelWidth; }
  
  /** Prints some debugging information. */
  virtual void PrintSelf(std::ostream &os, Indent i) const
  {
    os << i << "GaussianOperator { this=" << this
       << ", m_Variance = " << m_Variance
       << ", m_MaximumError = " << m_MaximumError
       << "} "  << std::endl;
    Superclass::PrintSelf(os, i.GetNextIndent());
  }
  
protected:
  typedef typename Superclass::CoefficientVector CoefficientVector;

  /** Returns the value of the modified Bessel function I0(x) at a point x >= 0. */
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
  void Fill(const CoefficientVector& coeff)
  {    this->FillCenteredDirectional(coeff);  }

private:
  /** Desired variance of the discrete Gaussian function. */
  double m_Variance;
  
  /** Difference between the areas under the curves of the continuous and
   * discrete Gaussian functions. */
  double m_MaximumError;

  /** Maximum kernel size allowed.  This value is used to truncate a kernel 
   *  that has grown too large.  A warning is given when the specified maximum
   *  error causes the kernel to exceed this size. */
  unsigned int m_MaximumKernelWidth;

  /** For compatibility with itkWarningMacro */
  const char *GetNameOfClass()
    { return "itkGaussianOperator"; }
  
};

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaussianOperator.txx"
#endif

#endif
