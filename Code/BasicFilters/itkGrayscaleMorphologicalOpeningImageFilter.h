/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGrayscaleMorphologicalOpeningImageFilter.h
  Language:  C++

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGrayscaleMorphologicalOpeningImageFilter_h
#define __itkGrayscaleMorphologicalOpeningImageFilter_h

#include "itkMorphologyImageFilter.h"

namespace itk {

/**
 * \class GrayscaleMorphologicalOpeningImageFilter
 * \brief gray scale morphological opening of an image.
 *
 * This filter preserves regions, in the foreground, that can
 * completely contain the structuring element. At the same time,
 * this filter eliminates all other regions of foreground 
 * pixels. The morphological opening of an image "f"
 * is defined as:
 * Opening(f) = Dilation(Erosion(f)).
 *
 * The structuring element is assumed to be composed of binary
 * values (zero or one). Only elements of the structuring element
 * having values > 0 are candidates for affecting the center pixel.
 * 
 *
 * \author Lino Ramirez. Dept. of Electrical and Computer Engineering. University of Alberta. Canada
 *
 * \sa MorphologyImageFilter, GrayscaleDilateImageFilter, GrayscaleErodeImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */

template<class TInputImage, class TOutputImage, class TKernel>
class ITK_EXPORT GrayscaleMorphologicalOpeningImageFilter : 
    public MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
{
public:
  /** Standard class typedefs. */
  typedef GrayscaleMorphologicalOpeningImageFilter Self;
  typedef MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Standard New method. */
  itkNewMacro(Self);  

  /** Runtime information support. */
  itkTypeMacro(GrayscaleMorphologicalOpeningImageFilter, 
               MorphologyImageFilter);

  typedef TInputImage InputImageType;
  typedef TOutputImage OutputImageType;
  typedef typename InputImageType::Pointer         InputImagePointer;
  typedef typename OutputImageType::RegionType     OutputImageRegionType;

  /** Declaration of pixel type. */
  typedef typename Superclass::PixelType PixelType;

  /** Kernel (structuring element) iterator. */
  typedef typename Superclass::KernelIteratorType  KernelIteratorType;

  /** Neighborhood iterator type. */
  typedef typename Superclass::NeighborhoodIteratorType NeighborhoodIteratorType ;

  /** Kernel typedef. */
  typedef typename Superclass::KernelType KernelType;

  /** Typedef for boundary conditions. */
  typedef typename Superclass::DefaultBoundaryConditionType DefaultBoundaryConditionType;
  typedef typename Superclass::ImageBoundaryConditionPointerType ImageBoundaryConditionPointerType;

  /** Set kernel (structuring element). */
  itkSetMacro(Kernel, KernelType);

protected:
  GrayscaleMorphologicalOpeningImageFilter();
  ~GrayscaleMorphologicalOpeningImageFilter() {};

  /** GrayscaleMorphologicalOpeningImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ;

  /** GrayscaleMorphologicalOpeningImageFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion(DataObject *itkNotUsed(output));

  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData (const OutputImageRegionType& 
                              outputRegionForThread,
                              int threadId) ;

  /** Evaluate image neighborhood with kernel to find the new value 
   * for the center pixel value
   *
   * It will return the maximum value of the image pixels whose corresponding
   * element in the structuring element is positive. This version of
   * Evaluate is used for non-boundary pixels. */
  PixelType Evaluate(const NeighborhoodIteratorType &nit,
                     const KernelIteratorType kernelBegin,
                     const KernelIteratorType kernelEnd);

private:
  GrayscaleMorphologicalOpeningImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  // Default boundary condition for dilation filter, defaults to
  // NumericTraits<PixelType>::NonpositiveMin()
  DefaultBoundaryConditionType m_MorphologicalOpeningBoundaryCondition;

  /** Pointer to a persistent boundary condition object used
   * for the image iterator. */
  ImageBoundaryConditionPointerType m_BoundaryCondition;


  /** kernel or structuring element to use. */
  KernelType m_Kernel ;

} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGrayscaleMorphologicalOpeningImageFilter.txx"
#endif

#endif


