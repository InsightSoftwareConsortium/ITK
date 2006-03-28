/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGrayscaleMorphologicalClosingImageFilter.h
  Language:  C++

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGrayscaleMorphologicalClosingImageFilter_h
#define __itkGrayscaleMorphologicalClosingImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk {

/**
 * \class GrayscaleMorphologicalClosingImageFilter
 * \brief gray scale morphological closing of an image.
 *
 * This filter removes small (i.e., smaller than the structuring 
 * element)holes and tube like structures in the interior or at the
 * boundaries of the image. The morphological closing of an image
 * "f" is defined as:
 * Closing(f) = Erosion(Dilation(f)).
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
class ITK_EXPORT GrayscaleMorphologicalClosingImageFilter : 
    public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef GrayscaleMorphologicalClosingImageFilter Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Standard New method. */
  itkNewMacro(Self);  

  /** Runtime information support. */
  itkTypeMacro(GrayscaleMorphologicalClosingImageFilter, 
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
  GrayscaleMorphologicalClosingImageFilter();
  ~GrayscaleMorphologicalClosingImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** GrayscaleMorphologicalClosingImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ;

  /** GrayscaleMorphologicalClosingImageFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion(DataObject *itkNotUsed(output));

  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleDilateImageFilter GrayscaleErodeImageFilter. */
  void  GenerateData ();

private:
  GrayscaleMorphologicalClosingImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** kernel or structuring element to use. */
  KernelType m_Kernel ;
} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGrayscaleMorphologicalClosingImageFilter.txx"
#endif

#endif


