/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRecursiveGaussianImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRecursiveGaussianImageFilter_h
#define __itkRecursiveGaussianImageFilter_h

#include "itkRecursiveSeparableImageFilter.h"

namespace itk
{
  
/** \class RecursiveGaussianImageFilter
 * \brief Base class for computing IIR convolution with an approximation of a  Gaussian kernel.
 *
 *    \f[    
 *      \frac{ 1 }{ \sigma \sqrt{ 2 \pi } } \exp{ \left( - \frac{x^2}{ 2 \sigma^2 } \right) }
 *    \f]
 *
 * RecursiveGaussianImageFilter is the base class for recursive filters that
 * approximate convolution with the Gaussian kernel.
 * This class implements the recursive filtering
 * method proposed by R.Deriche in IEEE-PAMI
 * Vol.12, No.1, January 1990, pp 78-87,
 * "Fast Algorithms for Low-Level Vision" 
 *
 * Details of the implementation are described in the technical report:
 * R. Deriche, "Recursively Implementing The Gaussian and Its Derivatives",
 * INRIA, 1993, ftp://ftp.inria.fr/INRIA/tech-reports/RR/RR-1893.ps.gz
 *
 * Further improvements of the algorithm are described in:
 * G. Farneback & C.-F. Westin, "On Implementation of Recursive Gaussian
 * Filters", so far unpublished.
 *
 * As compared to itk::DiscreteGaussianImageFilter, this filter tends
 * to be faster for large kernels, and it can take the derivative
 * of the blurred image in one step.  Also, note that we have
 * itk::RecursiveGaussianImageFilter::SetSigma(), but
 * itk::DiscreteGaussianImageFilter::SetVariance().
 * 
 * \ingroup ImageEnhancement Singlethreaded
 */
template <typename TInputImage, typename TOutputImage=TInputImage>
class ITK_EXPORT RecursiveGaussianImageFilter :
    public RecursiveSeparableImageFilter<TInputImage,TOutputImage> 
{
public:
  /** Standard class typedefs. */
  typedef RecursiveGaussianImageFilter  Self;
  typedef RecursiveSeparableImageFilter<TInputImage,TOutputImage> Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  typedef typename Superclass::RealType      RealType;
  typedef typename Superclass::ScalarRealType      ScalarRealType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Type macro that defines a name for this class */
  itkTypeMacro( RecursiveGaussianImageFilter, RecursiveSeparableImageFilter );

  /** Set/Get the Sigma, measured in world coordinates, of the Gaussian
   * kernel.  The default is 1.0.  */   
  itkGetMacro( Sigma, ScalarRealType );
  itkSetMacro( Sigma, ScalarRealType );

  /** Enum type that indicates if the filter applies the equivalent operation
      of convolving with a gaussian, first derivative of a gaussian or the 
      second derivative of a gaussian.  */
  typedef  enum { ZeroOrder, FirstOrder, SecondOrder } OrderEnumType;
 
  /** Type of the output image */
  typedef TOutputImage      OutputImageType;


  /** Set/Get the flag for normalizing the gaussian over scale space.
      When this flag is ON the filter will be normalized in such a way 
      that larger sigmas will not result in the image fading away.

      \f[    
            \frac{ 1 }{ \sqrt{ 2 \pi } };
      \f]

      When the flag is OFF the normalization will conserve contant the 
      integral of the image intensity. 
      \f[    
            \frac{ 1 }{ \sigma  \sqrt{ 2 \pi } };
      \f]
      For analyzing an image across Scale Space you want to enable
      this flag.  It is disabled by default.  */
  itkSetMacro( NormalizeAcrossScale, bool );
  itkGetMacro( NormalizeAcrossScale, bool );

  /** Set/Get the Order of the Gaussian to convolve with. 
      \li ZeroOrder is equivalent to convolving with a Gaussian.  This
      is the default.
      \li FirstOrder is equivalent to convolving with the first derivative of a Gaussian.
      \li SecondOrder is equivalent to convolving with the second derivative of a Gaussian.
    */
  itkSetMacro( Order, OrderEnumType );
  itkGetMacro( Order, OrderEnumType );

  /** Explicitly set a zeroth order derivative. */
  void SetZeroOrder();

  /** Explicitly set a first order derivative. */
  void SetFirstOrder();

  /** Explicitly set a second order derivative. */
  void SetSecondOrder();
  
   
protected:
  RecursiveGaussianImageFilter();
  virtual ~RecursiveGaussianImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Set up the coefficients of the filter to approximate a specific kernel.
   * Here it is used to approximate a Gaussian or one of its
   * derivatives. Parameter is the spacing along the dimension to
   * filter. */
  virtual void SetUp(ScalarRealType spacing);

private:  
  RecursiveGaussianImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Compute the N coefficients in the recursive filter. */
  void ComputeNCoefficients(ScalarRealType sigmad,
          ScalarRealType A1, ScalarRealType B1, ScalarRealType W1, ScalarRealType L1,
          ScalarRealType A2, ScalarRealType B2, ScalarRealType W2, ScalarRealType L2,
          ScalarRealType& N0, ScalarRealType& N1,
          ScalarRealType& N2, ScalarRealType& N3,
          ScalarRealType& SN, ScalarRealType& DN, ScalarRealType& EN);
  /** Compute the D coefficients in the recursive filter. */
  void ComputeDCoefficients(ScalarRealType sigmad,
          ScalarRealType W1, ScalarRealType L1, ScalarRealType W2, ScalarRealType L2,
          ScalarRealType& SD, ScalarRealType& DD, ScalarRealType& ED);
  /** Compute the M coefficients and the boundary coefficients in the
   * recursive filter. */
  void ComputeRemainingCoefficients(bool symmetric);

  /** Sigma of the gaussian kernel. */   
  ScalarRealType m_Sigma;

  /** Normalize the image across scale space */
  bool m_NormalizeAcrossScale; 

  OrderEnumType m_Order;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRecursiveGaussianImageFilter.txx"
#endif

#endif

