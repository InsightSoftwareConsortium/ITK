/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRecursiveSeparableImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRecursiveSeparableImageFilter_h
#define __itkRecursiveSeparableImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
  
/** \class RecursiveSeparableImageFilter
 * \brief Base class for recursive convolution with a kernel.
 *
 * RecursiveSeparableImageFilter is the base class for recursive 
 * filters that are applied in each dimension separatedly.
 * 
 * This class implements the recursive filtering
 * method proposed by R.Deriche in IEEE-PAMI
 * Vol.12, No.1, January 1990, pp 78-87.
 * 
 * \ingroup ImageFilters
 */
template <class TInputImage, class TOutputImage, class TComputation>
class ITK_EXPORT RecursiveSeparableImageFilter :
    public ImageToImageFilter<TInputImage,TOutputImage> 
{
public:
  /** Standard class typedefs. */
  typedef RecursiveSeparableImageFilter  Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>   Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Type macro that defines a name for this class. */
  itkTypeMacro( RecursiveSeparableImageFilter, ImageToImageFilter );

  /** Smart pointer typedef support.  */
  typedef typename TInputImage::Pointer  InputImagePointer;
  typedef typename TInputImage::ConstPointer  InputImageConstPointer;

  /** Get the direction in which the filter is to be applied. */   
  itkGetMacro(Direction, unsigned int);

  /** Set the direction in which the filter is to be applied. */   
  itkSetMacro(Direction, unsigned int);

  /** Set Input Image. */
  void SetInputImage( const TInputImage * );
    
  /** Get Input Image. */
  const TInputImage * GetInputImage( void );

protected:
  RecursiveSeparableImageFilter();
  virtual ~RecursiveSeparableImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** GenerateData (apply) the filter. */   
  void GenerateData(void);

  /** Set up the coefficients of the filter to approximate a specific kernel.
   * typically it can be used to approximate a gaussian or one of its
   * derivatives. */
  virtual void SetUp(void) = 0;

  /** Apply the recursive filter along one of the dimensions of the image.
   * This allow to filter each one of the dimensions of an image separately.
   * Sigma is given in length units so the spacing between pixels is taken 
   * into account. This is relevant for anisotropic images. */
  void ApplyRecursiveFilter(unsigned int dimension);

  /** Compute Recursive Filter Coefficients this method prepares the values of
   * the coefficients used for filtering the image. The symmetric flag is
   * used to enforce that the filter will be symmetric or antisymmetric. For
   * example, the Gaussian kernel is symmetric, while its first derivative is
   * antisymmetric. */
  virtual void ComputeFilterCoefficients(bool symmetric) = 0;

  /** Apply the Recursive Filter in an array of data.  this method is called
   * for each line of the volume from ApplyRecursiveFilter.
   * \sa ApplyRecursiveFilter.  */
  void FilterDataArray(TComputation *outs,
                       const TComputation *data, unsigned int ln);

private:  
  RecursiveSeparableImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Direction in which the filter is to be applied
   * this shoul in the range [0,ImageDimension-1] */ 
  unsigned int m_Direction;

protected:
  /**  Normalization factor. */
  TComputation m_K;                       

  /** Spacing along the direction of filtering. */   
  TComputation m_Spacing;

  /**  Parameter of exponential series. */
  TComputation m_A0;
  TComputation m_A1;
  TComputation m_B0;
  TComputation m_B1;
  TComputation m_C0;
  TComputation m_C1;
  TComputation m_W0;
  TComputation m_W1; 

  /** Causal coefficients that multiply the input data. */
  TComputation m_N00;
  TComputation m_N11;
  TComputation m_N22;
  TComputation m_N33; 
  
  /** Recursive coefficients that multiply previously computed values at the output.
      In this case the Causal coefficients == Anticausal coefficients. */
  TComputation m_D11;
  TComputation m_D22;
  TComputation m_D33;
  TComputation m_D44; 
  
  /** Anti-Causal coefficients (symmetric case). that multiply the input data */
  TComputation m_M11;
  TComputation m_M22;
  TComputation m_M33;
  TComputation m_M44; 

  /** Recursive coefficients to be used at the boundaries to prevent border effects */
  TComputation m_BN1;
  TComputation m_BN2;
  TComputation m_BN3;
  TComputation m_BN4; 
 
  TComputation m_BM1;
  TComputation m_BM2;
  TComputation m_BM3;
  TComputation m_BM4; 
 
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRecursiveSeparableImageFilter.txx"
#endif


#endif
