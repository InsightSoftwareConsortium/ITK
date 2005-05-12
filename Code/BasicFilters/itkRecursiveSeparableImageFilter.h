/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRecursiveSeparableImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
 * filters that are applied in each dimension separately.
 *
 * This class implements the recursive filtering
 * method proposed by R.Deriche in IEEE-PAMI
 * Vol.12, No.1, January 1990, pp 78-87.
 * 
 * Details of the implementation are described in the technical report:
 * R. Deriche, "Recursively Implementing The Gaussian and Its Derivatives",
 * INRIA, 1993, ftp://ftp.inria.fr/INRIA/tech-reports/RR/RR-1893.ps.gz
 *
 * Further improvements of the algorithm are described in:
 * G. Farneback & C.-F. Westin, "On Implementation of Recursive Gaussian
 * Filters", so far unpublished.
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

  /** Real type to be used in internal computations. RealType in general is
   * templated over the pixel type. (For example for vector or tensor pixels,
   * RealType is a vector or a tensor of doubles.) ScalarRealType is a type 
   * meant for scalars.*/
  typedef typename TInputImage::PixelType                   InputPixelType;
  typedef typename NumericTraits<InputPixelType>::RealType  RealType;
  typedef typename NumericTraits<InputPixelType>::ScalarRealType ScalarRealType;

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

protected:
  RecursiveSeparableImageFilter();
  virtual ~RecursiveSeparableImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** GenerateData (apply) the filter. */   
  void GenerateData(void);

  /** RecursiveSeparableImageFilter needs all of the input to produce an
   * output. Therefore, RecursiveSeparableImageFilter needs to provide
   * an implementation for GenerateInputRequestedRegion in order to inform
   * the pipeline execution model.
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

  // Override since the filter produces the entire dataset.
  void EnlargeOutputRequestedRegion(DataObject *output);

  /** Set up the coefficients of the filter to approximate a specific kernel.
   * Typically it can be used to approximate a Gaussian or one of its
   * derivatives. Parameter is the spacing along the dimension to
   * filter. */
  virtual void SetUp(ScalarRealType spacing) = 0;

  /** Apply the Recursive Filter to an array of data.  This method is called
   * for each line of the volume. Parameter "scratch" is a scratch
   * area used for internal computations that is the same size as the
   * parameters "outs" and "data". The scratch area must be allocated
   * outside of this routine (this avoids memory allocation and
   * deallocation in the inner loop of the overall algorithm. */
  void FilterDataArray(RealType *outs, const RealType *data, RealType *scratch,
                       unsigned int ln);

private:  
  RecursiveSeparableImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Direction in which the filter is to be applied
   * this should be in the range [0,ImageDimension-1]. */ 
  unsigned int m_Direction;

protected:
  /** Causal coefficients that multiply the input data. */
  ScalarRealType m_N0;
  ScalarRealType m_N1;
  ScalarRealType m_N2;
  ScalarRealType m_N3; 
  
  /** Recursive coefficients that multiply previously computed values
   * at the output. These are the same for the causal and
   * anti-causal parts of the filter. */
  ScalarRealType m_D1;
  ScalarRealType m_D2;
  ScalarRealType m_D3;
  ScalarRealType m_D4; 
  
  /** Anti-causal coefficients that multiply the input data. */
  ScalarRealType m_M1;
  ScalarRealType m_M2;
  ScalarRealType m_M3;
  ScalarRealType m_M4; 

  /** Recursive coefficients to be used at the boundaries to simulate
   * edge extension boundary conditions. */
  ScalarRealType m_BN1;
  ScalarRealType m_BN2;
  ScalarRealType m_BN3;
  ScalarRealType m_BN4; 
 
  ScalarRealType m_BM1;
  ScalarRealType m_BM2;
  ScalarRealType m_BM3;
  ScalarRealType m_BM4; 
 
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRecursiveSeparableImageFilter.txx"
#endif


#endif
