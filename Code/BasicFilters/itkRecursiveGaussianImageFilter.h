/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRecursiveGaussianImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
 * \brief Base class for recursive convolution with Gaussian kernel.
 *
 * RecursiveGaussianImageFilter is the base class for recursive filters that
 * approximate convolution with the Gaussian kernel.
 * This class implements the recursive filtering
 * method proposed by R.Deriche in IEEE-PAMI
 * Vol.12, No.1, January 1990, pp 78-87.
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

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Type macro that defines a name for this class */
  itkTypeMacro( RecursiveGaussianImageFilter, RecursiveSeparableImageFilter );

  /** Set/Get the Sigma, measured in world coordinates, of the Gaussian
   * kernel. */   
  itkGetMacro( Sigma, RealType );
  itkSetMacro( Sigma, RealType );

  /** Enum type that indicates if the filter applies the equivalent operation
      of convolving with a gaussian, first derivative of a gaussian or the 
      second derivative of a gaussian.  */
  typedef  enum { ZeroOrder, FirstOrder, SecondOrder } OrderEnumType;
 
  /** Type of the output image */
  typedef TOutputImage      OutputImageType;


  /** Set/Get the flag for normalizing the gaussian over scale Space
      When this flag is ON the filter will be normalized in such a way 
      that larger sigmas will not result in the image fading away.

      \f[    
            \frac{ 1 }{ \sigma  sqrt{ 2 \pi } };
      \f]

      When the flag is OFF the normalization will conserve contant the 
      integral of the image intensity. 
      \f[    
            \frac{ 1 }{ \sigma^2  sqrt{ 2 \pi } };
      \f]
      For analyzing an image across Scale Space you want to enable this flag.  */
  itkSetMacro( NormalizeAcrossScale, bool );
  itkGetMacro( NormalizeAcrossScale, bool );

  /** Set/Get the Order of the Gaussian to convolve with. 
      \li ZeroOrder is equivalent to convolving with a Gaussian
      \li FirstOrder is equivalet to convolving with the first derivative of a Gaussian
      \li SecondOrder is equivalet to convolving with the second derivative of a Gaussian
    */
   itkSetMacro( Order, OrderEnumType );
   itkGetMacro( Order, OrderEnumType );
  
   
protected:
  RecursiveGaussianImageFilter();
  virtual ~RecursiveGaussianImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Set up the coefficients of the filter to approximate a specific kernel.
   * typically it can be used to approximate a gaussian or one of its
   * derivatives. */
  virtual void SetUp(void);

   /** Compute Recursive Filter Coefficients this method prepares the values of
   * the coefficients used for filtering the image. The symmetric flag is
   * used to enforce that the filter will be symmetric or antisymmetric. For
   * example, the Gaussian kernel is symmetric, while its first derivative is
   * antisymmetric. */
  void ComputeFilterCoefficients(bool symmetric);

private:  
  RecursiveGaussianImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Sigma of the gaussian kernel. */   
  RealType m_Sigma;

  /** Normalize the image across scale space */
  bool m_NormalizeAcrossScale; 

  OrderEnumType   m_Order;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRecursiveGaussianImageFilter.txx"
#endif

#endif
