/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMorphologyImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMorphologyImageFilter_h
#define __itkMorphologyImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNeighborhoodIterator.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhood.h"
#include "itkConstSliceIterator.h"
#include "itkImageBoundaryCondition.h"
#include "itkImageRegionIterator.h"

namespace itk {

/** \class MorphologyImageFilter 
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
  /** Standard Self typedef */
  typedef MorphologyImageFilter Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Runtime information support. */
  itkTypeMacro(MorphologyImageFilter, ImageToImageFilter);
  
  /** Image related typedefs. */
  typedef TInputImage InputImageType;
  typedef TOutputImage OutputImageType;
  typedef typename TInputImage::RegionType RegionType ;
  typedef typename TInputImage::SizeType SizeType ;
  typedef typename TInputImage::IndexType IndexType ;
  typedef typename TInputImage::PixelType PixelType ;
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;
  
  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Neighborhood iterator type. */
  typedef ConstNeighborhoodIterator<TInputImage> 
    NeighborhoodIteratorType ;

  /** Kernel typedef. */
  typedef TKernel KernelType;
  
  /** Kernel (structuring element) iterator. */
  typedef typename KernelType::ConstIterator KernelIteratorType ;
  
  /** n-dimensional Kernel radius. */
  typedef typename KernelType::SizeType RadiusType ;

  /** Set kernel (structuring element). */
  itkSetMacro(Kernel, KernelType);

  /** Get the kernel (structuring element). */
  itkGetConstReferenceMacro(Kernel, KernelType);
  
  /** MorphologyImageFilters need to make sure they request enough of an
   * input image to account for the structuring element size.  The input
   * requested region is expanded by the radius of the structuring element.
   * If the request extends past the LargestPossibleRegion for the input,
   * the request is cropped by the LargestPossibleRegion. */
  void GenerateInputRequestedRegion() ;

protected:
  MorphologyImageFilter();
  ~MorphologyImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData (const OutputImageRegionType& 
                              outputRegionForThread,
                              int threadId) ;

  /** Evaluate image neighborhood with kernel to find the new value 
   * for the center pixel value. */
  virtual PixelType Evaluate(const NeighborhoodIteratorType &nit,
                             const KernelIteratorType kernelBegin,
                             const KernelIteratorType kernelEnd)=0;

private:
  MorphologyImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** kernel or structuring element to use. */
  KernelType m_Kernel ;

} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMorphologyImageFilter.txx"
#endif

#endif
