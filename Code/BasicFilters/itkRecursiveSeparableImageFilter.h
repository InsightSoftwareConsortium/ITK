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
#include "itkNumericTraits.h"

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
template <typename TInputImage, typename TOutputImage=TInputImage>
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

  /** Real type to be used in internal computations */
  typedef typename TInputImage::PixelType                   InputPixelType;
  typedef typename NumericTraits<InputPixelType>::RealType  RealType;

  /** Type of the input image */
  typedef TInputImage      InputImageType;

  /** Type of the output image */
  typedef TOutputImage      OutputImageType;

  /** Get the direction in which the filter is to be applied. */   
  itkGetMacro(Direction, unsigned int);

  /** Set the direction in which the filter is to be applied. */   
  itkSetMacro(Direction, unsigned int);

  /** Set Input Image. */
  void SetInputImage( const TInputImage * );
    
  /** Get Input Image. */
  const TInputImage * GetInputImage( void );

  /** RecursiveSeparableImageFilter needs all of the input to produce an
   * output. Therefore, RecursiveSeparableImageFilter needs to provide
   * an implementation for GenerateInputRequestedRegion in order to inform
   * the pipeline execution model.
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

protected:
  RecursiveSeparableImageFilter();
  virtual ~RecursiveSeparableImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** GenerateData (apply) the filter. */   
  void GenerateData(void);

  // Override since the filter produces the entire dataset
  void EnlargeOutputRequestedRegion(DataObject *output);

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
  void FilterDataArray(RealType *outs,
                 const RealType *data, unsigned int ln);

private:  
  RecursiveSeparableImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Direction in which the filter is to be applied
   * this shoul in the range [0,ImageDimension-1] */ 
  unsigned int m_Direction;

protected:
  /**  Normalization factor. */
  RealType m_K;                       

  /** Spacing along the direction of filtering. */   
  RealType m_Spacing;

  /**  Parameter of exponential series. */
  RealType m_A0;
  RealType m_A1;
  RealType m_B0;
  RealType m_B1;
  RealType m_C0;
  RealType m_C1;
  RealType m_W0;
  RealType m_W1; 

  /** Causal coefficients that multiply the input data. */
  RealType m_N00;
  RealType m_N11;
  RealType m_N22;
  RealType m_N33; 
  
  /** Recursive coefficients that multiply previously computed values at the output.
      In this case the Causal coefficients == Anticausal coefficients. */
  RealType m_D11;
  RealType m_D22;
  RealType m_D33;
  RealType m_D44; 
  
  /** Anti-Causal coefficients (symmetric case). that multiply the input data */
  RealType m_M11;
  RealType m_M22;
  RealType m_M33;
  RealType m_M44; 

  /** Recursive coefficients to be used at the boundaries to prevent border effects */
  RealType m_BN1;
  RealType m_BN2;
  RealType m_BN3;
  RealType m_BN4; 
 
  RealType m_BM1;
  RealType m_BM2;
  RealType m_BM3;
  RealType m_BM4; 
 
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRecursiveSeparableImageFilter.txx"
#endif


#endif
