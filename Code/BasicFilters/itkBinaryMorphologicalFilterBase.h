/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryMorphologicalFilterBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkBinaryMorphologicalFilterBase_h
#define __itkBinaryMorphologicalFilterBase_h

#include "itkImageToImageFilter.h"
#include "itkNeighborhoodIterator.h"
#include "itkConstSmartNeighborhoodIterator.h"
#include "itkNeighborhood.h"
#include "itkConstSliceIterator.h"
#include "itkImageBoundaryCondition.h"
#include "itkImageRegionIterator.h"

namespace itk {

  /** \class BinaryMorphologicalFilterBase 
   * \brief implementation of the base class for the binary morphological
   * operation such as erosion and dialation
   *
   * This class provides most of fuctionalities for binary 
   * morphological operations. Any sub class of this class expects an gray 
   * value image of which pixel data are separable into background pixels and  
   *  forground using a threshold value (see SetThreshold).
   *
   * Subclasses of this class can define their own operation by simply
   * providing their own Evaluate() protected member function.
   *
   *
   * NOTE:
   * this class hasn't been tested over the case where the input requested
   * region is different from the output requested region
   *
   * \sa BinaryMorphologicalErosionFilter
   * \sa BinaryMorphologicalDialationFilter
   * \sa NeighborhoodIterator
   * \sa Neighborhood
   * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
   */

template<class TInputImage, class TOutputImage, class TKernel>
class ITK_EXPORT BinaryMorphologicalFilterBase : 
  public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /**
   * Standard Self typedef
   */
  typedef BinaryMorphologicalFilterBase Self;

  /**
   * Standard Superclass typedef
   */
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;

  /**
   * Standard smart pointer support
   */ 
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * Runtime information support
   */
  itkTypeMacro(BinaryMorphologicalFilterBase, ImageToImageFilter);
  
  /**
   * Standard New method
   */
  itkNewMacro(Self);  

  /**
   * Image related typedefs
   */
  typedef typename TInputImage::RegionType RegionType ;
  typedef typename TInputImage::SizeType SizeType ;
  typedef typename TInputImage::IndexType IndexType ;
  typedef typename TInputImage::PixelType PixelType ;
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;
  
  enum { ImageDimension = TInputImage::ImageDimension } ;

  /**
   * Neighborhood iterator type
   */
  typedef ConstSmartNeighborhoodIterator<TInputImage> 
    NeighborhoodIteratorType ;

  /**
   * Iterator type for the neighborhood that 
   * NeighborhoodIteratorType::GetNeighborhood() will return 
   */
  typedef typename NeighborhoodIteratorType::NeighborhoodType::ConstIterator 
    ImageKernelIteratorType ;

  /**
   * Kernel (structuring elemnent) iterator
   */
  typedef typename TKernel::ConstIterator KernelIteratorType ;
  
  /**
   * n-dimensional Kernel radius
   */
  typedef typename TKernel::SizeType RadiusType ;

  /**
   * Set kernel (structuring element)
   */
  void SetKernel(TKernel* kernel) ;

  /**
   * Set threshold value which will be used to separate background from image
   */
  void SetThreshold(PixelType threshold) ;

  /**
   * Make sure that the input requested region should include the output 
   * requested region + kernel radius 
   */
  void GenerateInputRequestedRegion() ;

protected:
  /**
   * Multi-thread version GenerateData
   */
  void  ThreadedGenerateData (const OutputImageRegionType& 
                              outputRegionForThread,
                              int threadId) ;

  /**
   * Evaluate image neighborhood with kernel to find the new value 
   * for the center pixel value
   */
  virtual PixelType Evaluate(ImageKernelIteratorType first, 
                             ImageKernelIteratorType last, 
                             KernelIteratorType first2,
                             PixelType centerValue,
                             PixelType threshold) = 0 ;

  /**
   * Return the region that includes region (output requested region)
   * + radius (kernel radius)
   */
  RegionType EnlargeImageRegion(RegionType region,
                                RadiusType radius) ;

  /**
   * Return the region that is big enough to include the new requested region
   * after the above enlargement but smaller than the largest possible region
   * of the input image. 
   *
   * NOTE:
   * If the current input requested region includes athe new requested region 
   *, then it keeps the original requested region
   */
  RegionType EnlargeImageRegion(RegionType current,
                                RegionType largest,
                                RegionType requested) ;

private:
  /**
   * the kernel pointer
   */
  TKernel* m_Kernel ;

  /**
   * threshold value
   */
  PixelType m_Threshold ;
} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryMorphologicalFilterBase.txx"
#endif

#endif
