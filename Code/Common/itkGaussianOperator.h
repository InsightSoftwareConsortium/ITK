/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianOperator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#ifndef __itkGaussianOperator_h
#define __itkGaussianOperator_h

#include "itkNeighborhoodOperator.h"
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
 */
template<unsigned int VDimension=2>
class ITK_EXPORT GaussianOperator
  : public NeighborhoodOperator<float, VDimension>
{
public:
  /**
   * Run-time type information
   */
  itkTypeMacro(GaussianOperator, NeighborhoodOperator);

  /**
   * Standard "Self" typedef support.
   */
  typedef GaussianOperator Self;

  /**
   * NeighborhoodOperator typedef support.
   */
  typedef NeighborhoodOperator<float, VDimension> NeighborhoodOperator;

  /**
   * Constructor.
   */
  GaussianOperator() : m_Variance(1), m_MaximumError(.01) { }

  /**
   * Sets the desired variance of the Gaussian kernel.
   */
  void SetVariance(const float &variance)
  {
    m_Variance = variance;
  }

  /**
   * Sets the desired maximum error of the gaussian approximation.  Maximum
   * error is the difference between the area under the discrete Gaussian curve
   * and the area under the continuous Gaussian. Maximum error affects the
   * Gaussian operator size.
   */
  void SetMaximumError(const float &max_error)
  {
    // --- FIX EXCEPTION HANDLING ---
    if (m_MaximumError >= 1 || m_MaximumError <= 0)
      {
        throw ExceptionObject();
      }
    
    m_MaximumError = max_error;
  }
  
  /**
   * Returns the variance of the Gaussian (scale) for the operator.
   */
  float GetVariance()
  {
    return m_Variance;
  }

  /**
   * Returns the maximum error of the gaussian approximation.  Maximum error is 
   * the difference between the area under the discrete Gaussian curve and the
   * area under the continuous Gaussian. Maximum error affects the Gaussian
   * operator size.
   */
  float GetMaximumError()
  {
    return m_MaximumError;
  }

  /**
   * Prints some debugging information.
   */
  void PrintSelf()                // Note: This method is for debugging/devel
  {                           //  purposes and should probably be removed
                              // at some point.  jc 10-06-00
    NeighborhoodOperator::PrintSelf();
    std::cout << "GaussianOperator" << std::endl;
    std::cout << "\t Variance = " << m_Variance << std::endl;
    std::cout << "\t MaximumError = " << m_MaximumError << std::endl;
  }
  
protected:
  typedef std::vector<float> CoefficientVector;

  /**
   * Returns the value of the modified Bessel function I0(x) at a point x >= 0.
   */
  float ModifiedBesselI0(float);

  /**
   * Returns the value of the modified Bessel function I1(x) at a point x,
   * x real. 
   */
  float ModifiedBesselI1(float);

  /**
   * Returns the value of the modified Bessel function Ik(x) at a point x>=0,
   * where k>=2.
   */
  float ModifiedBesselI(int, float);

  /**
   * Calculates operator coefficients.
   */
  CoefficientVector GenerateCoefficients();

  /**
   * Arranges coefficients spatially in the memory buffer.
   */
  void Fill(const CoefficientVector& coeff)
  {
    this->FillCenteredDirectional(coeff);
  }

private:
  /**
   * Desired variance of the discrete Gaussian function.
   */
  float m_Variance;
  
  /**
   * Difference between the areas under the curves of the continuous and
   * discrete Gaussian functions.
   */
  float m_MaximumError;
  
};

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaussianOperator.txx"
#endif

#endif
