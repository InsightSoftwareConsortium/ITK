/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkObjectMorphologyImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkObjectMorphologyImageFilter_h
#define __itkObjectMorphologyImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNeighborhoodIterator.h"
#include "itkConstSmartNeighborhoodIterator.h"
#include "itkSmartNeighborhoodIterator.h"
#include "itkNeighborhood.h"
#include "itkConstSliceIterator.h"
#include "itkImageBoundaryCondition.h"
#include "itkImageRegionIterator.h"

namespace itk {

/** \class ObjectMorphologyImageFilter 
 * \brief Base class for the morphological operations 
 * being applied to isolated objects in an image.
 *
 * This class provides the infrastructure to support of
 * morphological operations being applied to images in which
 * the foreground and background intensities are fixed. This filter
 * operates significantly faster than itkBinaryMorhologicalImageFilters;
 * however itkBinaryMorhologicalImageFilters preserve
 * background pixels based on values of neighboring background 
 * pixels - potentially important during erosion.
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
 * providing their own Evaluate() protected member functions - one
 * that operates using a smart neighborhood operator for edge faces and
 * one that operates using a standard neighboorhood operator..
 *
 * \sa ErodeObjectMorphologicalImageFilter
 * \sa DilateObjectMorphologicalImageFilter
 * \sa BinaryErodeImageFilter
 * \sa BinaryDilateImageFilter
 * \sa GrayScaleErodeImageFilter
 * \sa GrayScaleDilateImageFilter
 * \sa SmartNeighborhoodIterator
 * \sa SmartNeighborhood
 * \sa NeighborhoodIterator
 * \sa Neighborhood
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template<class TInputImage, class TOutputImage, class TKernel>
class ITK_EXPORT ObjectMorphologyImageFilter : 
  public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard Self typedef */
  typedef ObjectMorphologyImageFilter Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Runtime information support. */
  itkTypeMacro(ObjectMorphologyImageFilter, ImageToImageFilter);
  
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
    InputNeighborhoodIteratorType ;
  typedef ConstSmartNeighborhoodIterator<TInputImage> 
    InputSmartNeighborhoodIteratorType ;

  typedef NeighborhoodIterator<TOutputImage> 
    OutputNeighborhoodIteratorType ;
  typedef SmartNeighborhoodIterator<TOutputImage> 
    OutputSmartNeighborhoodIteratorType ;

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

  /** Get the pixel value being used to identify the object of interest */
  itkGetMacro(ObjectValue, PixelType);

  /** Set the pixel value being used to identify the object of interest */
  itkSetMacro(ObjectValue, PixelType);
  
  /** ObjectMorphologyImageFilters need to make sure they request enough of an
   * input image to account for the structuring element size.  The input
   * requested region is expanded by the radius of the structuring element.
   * If the request extends past the LargestPossibleRegion for the input,
   * the request is cropped by the LargestPossibleRegion. */
  void GenerateInputRequestedRegion() ;

protected:
  ObjectMorphologyImageFilter();
  ~ObjectMorphologyImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData (const OutputImageRegionType& 
                              outputRegionForThread,
                              int threadId) ;

  /** Evaluate image neighborhood with kernel to find the new value 
   * for the center pixel value. This version is used for boundary
   * pixels - i.e., pixels in the first facelist of an image. */
  virtual void Evaluate(OutputSmartNeighborhoodIteratorType &nit,
                             const KernelType &kernel)=0;

  /** Evaluate image neighborhood with kernel to find the new value 
   * for the center pixel value. This version is used for non-boundary
   * pixels - i.e., pixels at which the kernel won't exit the image */
  virtual void Evaluate(OutputNeighborhoodIteratorType &nit,
                             const KernelType &kernel)=0;

  /** Evaluate a pixel (assumed to have a value of ObjectValue) to 
   * determine if one of its neighboring pixels (8-neigh in 2d, etc) is a 
   * non-ObjectValue pixel. This version is used for non-boundary
   * pixels - i.e., pixels at which the kernel won't exit the image */
  bool IsObjectPixelOnBoundary(const InputNeighborhoodIteratorType &nit);

  /** Evaluate a pixel (assumed to have a value of ObjectValue) to 
   * determine if one of its neighboring pixels (8-neigh in 2d, etc) is a 
   * non-ObjectValue pixel. This version is used for boundary
   * pixels - i.e., pixels at which the kernel will exit the image */
  bool IsObjectPixelOnBoundary(const InputSmartNeighborhoodIteratorType &nit);

  /** kernel or structuring element to use. */
  KernelType m_Kernel ;

  /** Pixel value that indicates the object be operated upon */
  PixelType m_ObjectValue;

private:
  ObjectMorphologyImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkObjectMorphologyImageFilter.txx"
#endif

#endif
