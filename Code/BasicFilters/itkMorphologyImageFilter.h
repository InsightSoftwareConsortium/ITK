/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMorphologyImageFilter.h
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
#ifndef __itkMorphologyImageFilter_h
#define __itkMorphologyImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNeighborhoodIterator.h"
#include "itkConstSmartNeighborhoodIterator.h"
#include "itkNeighborhood.h"
#include "itkConstSliceIterator.h"
#include "itkImageBoundaryCondition.h"
#include "itkImageRegionIterator.h"

namespace itk {

/**
 * \class MorphologyImageFilter 
 * \brief Base class for the morphological operations such as erosion and dialation
 *
 * This class provides the infrastructure to support most
 * morphological operations. Subclasses of MorphologyImageFilter
 * implement specific "binary" and "grayscale" operations.  The
 * "binary" subclasses can operate on gray level data, where a
 * specified a pixel value is consider the "foreground" and every
 * other pixel value is considered the background.  This is useful for
 * operating on segment masks where all pixels assigned to segment #1
 * have value 1, all pixels assigned to segment #2 have value 2, etc.
 * Here, a given segment can be dilated (expanded) while treating all
 * other segment identifiers are background.
 *
 * The "kernel" specified represents a morphology structuring element.
 * The structuring element is a small Neighborhood with values
 * indicating an element is "on" (value > 0) or "off" (value <=0).
 * Morphological operations are defined by placing the structuring
 * element over a pixel, and calculating a nonlinear function (min,
 * max) over the pixels of the image that are under pixels in the
 * structuring element that are "on". The result of this calculation
 * is the value of the pixel in the output image.  Under most
 * circumstances, the "center pixel" of the structuring element -- or
 * structuring element pixel over the input pixel under consideration
 * -- is prescribed to be "on". This is not a strict requirement but
 * the subclasses of this filter are not guarenteed to produce the
 * correct result if the "center pixel" is not part of the structuring
 * element.
 *
 * Subclasses of this class can define their own operations by simply
 * providing their own Evaluate() protected member function.
 *
 * \sa BinaryErodeImageFilter
 * \sa BinaryDilateImageFilter
 * \sa GrayScaleErodeImageFilter
 * \sa GrayScaleDilateImageFilter
 * \sa NeighborhoodIterator
 * \sa Neighborhood
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */

template<class TInputImage, class TOutputImage, class TKernel>
class ITK_EXPORT MorphologyImageFilter : 
  public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /**
   * Standard Self typedef
   */
  typedef MorphologyImageFilter Self;

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
  itkTypeMacro(MorphologyImageFilter, ImageToImageFilter);
  
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
    SmartNeighborhoodIteratorType ;

  /**
   * Kernel typedef
   */
  typedef TKernel KernelType;
  
  /**
   * Kernel (structuring element) iterator
   */
  typedef typename KernelType::ConstIterator KernelIteratorType ;
  
  /**
   * n-dimensional Kernel radius
   */
  typedef typename KernelType::SizeType RadiusType ;

  /**
   * Set kernel (structuring element)
   */
  itkSetMacro(Kernel, KernelType);

  /**
   * Get the kernel (structuring element)
   */
  itkGetConstReferenceMacro(Kernel, KernelType);
  
  /**
   * MorphologyImageFilters need to make sure they request enough of an
   * input image to account for the structuring element size.  The input
   * requested region is expanded by the radius of the structuring element.
   * If the request extends past the LargestPossibleRegion for the input,
   * the request is cropped by the LargestPossibleRegion.
   */
  void GenerateInputRequestedRegion() ;

protected:
  MorphologyImageFilter();
  ~MorphologyImageFilter() {};
  MorphologyImageFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

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
  virtual PixelType Evaluate(const SmartNeighborhoodIteratorType &nit,
                             const KernelType &kernel)=0;


private:
  /**
   * kernel or structuring element to use
   */
  KernelType m_Kernel ;

} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMorphologyImageFilter.txx"
#endif

#endif
