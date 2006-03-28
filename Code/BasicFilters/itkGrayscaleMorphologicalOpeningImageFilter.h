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

#include "itkImageToImageFilter.h"

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
    public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef GrayscaleMorphologicalOpeningImageFilter Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Standard New method. */
  itkNewMacro(Self);  

  /** Runtime information support. */
  itkTypeMacro(GrayscaleMorphologicalOpeningImageFilter, 
               ImageToImageFilter);

  typedef TInputImage InputImageType;
  typedef TOutputImage OutputImageType;
  typedef typename InputImageType::Pointer         InputImagePointer;
  typedef typename OutputImageType::RegionType     OutputImageRegionType;

  /** Declaration of pixel type. */
  typedef typename TInputImage::PixelType PixelType ;

  /** Kernel typedef. */
  typedef TKernel KernelType;

  /** Set kernel (structuring element). */
  itkSetMacro(Kernel, KernelType);

  /** Get the kernel (structuring element). */
  itkGetConstReferenceMacro(Kernel, KernelType);

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(KernelDimension, unsigned int,
                      TKernel::NeighborhoodDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(SameTypeCheck,
    (Concept::SameType<PixelType, typename TOutputImage::PixelType>));
  itkConceptMacro(SameDimensionCheck1,
    (Concept::SameDimension<InputImageDimension, OutputImageDimension>));
  itkConceptMacro(SameDimensionCheck2,
    (Concept::SameDimension<InputImageDimension, KernelDimension>));
  itkConceptMacro(InputLessThanComparableCheck,
    (Concept::LessThanComparable<PixelType>));
  itkConceptMacro(InputGreaterThanComparableCheck,
    (Concept::GreaterThanComparable<PixelType>));
  itkConceptMacro(KernelGreaterThanIntCheck,
    (Concept::GreaterThanComparable<typename TKernel::PixelType, int>));
  /** End concept checking */
#endif

protected:
  GrayscaleMorphologicalOpeningImageFilter();
  ~GrayscaleMorphologicalOpeningImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** GrayscaleMorphologicalOpeningImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ;

  /** GrayscaleMorphologicalOpeningImageFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion(DataObject *itkNotUsed(output));

  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleDilateImageFilter GrayscaleErodeImageFilter. */
  void  GenerateData ();

private:
  GrayscaleMorphologicalOpeningImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** kernel or structuring element to use. */
  KernelType m_Kernel ;

} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGrayscaleMorphologicalOpeningImageFilter.txx"
#endif

#endif


