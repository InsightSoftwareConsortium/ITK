/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMorphologicalGradientImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMorphologicalGradientImageFilter_h
#define __itkMorphologicalGradientImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk {

/** \class MorphologicalGradientImageFilter
 * \brief Morphological gradients enhance the variation of pixel intensity in a given neighborhood.
 *
 * The morphological gradient, also called the Beucher gradient, is the
 * difference between the dilation and erosion by the structuring element.
 * Morphological gradient is described in Chapter 3.8.1 of Pierre Soille's book 
 * "Morphological Image Analysis: Principles and Applications", 
 * Second Edition, Springer, 2003.
 * According to this book, this filter can produce thick gradients, by
 * increasing the size of the structuring element. Also, direcitonal
 * gradiebts are possible by replacing the isotropic structuring
 * element with a line segment.
 * The name might be MorphologicalGradientMagnitudeImageFilter, but we
 * chose to name the filter according to Pierre Soille's usage in the book.
 *
 * Note: This filter produces a scalar image that is the magnitude of
 * the gradient.
 * 
 * \author Gaëtan Lehmann. Biologie du Développement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa GradientMagnitudeImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template<class TInputImage, class TOutputImage, class TKernel>
class ITK_EXPORT MorphologicalGradientImageFilter : 
    public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef MorphologicalGradientImageFilter Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage>
  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage InputImageType;
  typedef TOutputImage OutputImageType;
  typedef typename InputImageType::Pointer         InputImagePointer;
  typedef typename InputImageType::ConstPointer    InputImageConstPointer;
  typedef typename InputImageType::RegionType      InputImageRegionType;
  typedef typename InputImageType::PixelType       InputImagePixelType;
  typedef typename OutputImageType::Pointer        OutputImagePointer;
  typedef typename OutputImageType::ConstPointer   OutputImageConstPointer;
  typedef typename OutputImageType::RegionType     OutputImageRegionType;
  typedef typename OutputImageType::PixelType      OutputImagePixelType;
  
 /** Kernel typedef. */
  typedef TKernel KernelType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(KernelDimension, unsigned int,
                      TKernel::NeighborhoodDimension);

  /** Standard New method. */
  itkNewMacro(Self);  

  /** Runtime information support. */
  itkTypeMacro(MorphologicalGradientImageFilter, 
               ImageToImageFilter);

  /** Set kernel (structuring element). */
  itkSetMacro(Kernel, KernelType);
  
  /** Get the kernel (structuring element). */
  itkGetConstReferenceMacro(Kernel, KernelType);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(SameDimensionCheck1,
    (Concept::SameDimension<InputImageDimension, KernelDimension>));
  itkConceptMacro(SameDimensionCheck2,
    (Concept::SameDimension<InputImageDimension, OutputImageDimension>));
  itkConceptMacro(InputLessThanComparableCheck,
    (Concept::LessThanComparable<InputImagePixelType>));
  itkConceptMacro(InputGreaterThanComparableCheck,
    (Concept::GreaterThanComparable<InputImagePixelType>));
  itkConceptMacro(InputAdditiveOperatorsCheck,
    (Concept::AdditiveOperators<InputImagePixelType>));
  itkConceptMacro(InputConvertibleToOutputCheck,
    (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
  itkConceptMacro(KernelGreaterThanIntCheck,
    (Concept::GreaterThanComparable<typename TKernel::PixelType, int>));
  /** End concept checking */
#endif

protected:
  MorphologicalGradientImageFilter();
  ~MorphologicalGradientImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** MorphologicalGradientImageFilter need to make sure they request
   * enough of an input image to account for the structuring element
   * size.  The input requested region is expanded by the radius of
   * the structuring element. If the request extends past the
   * LargestPossibleRegion for the input, the request is cropped by
   * the LargestPossibleRegion.
   */
  void GenerateInputRequestedRegion() ;

  void GenerateData();
  

private:
  MorphologicalGradientImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** kernel or structuring element to use. */
  KernelType m_Kernel ;
} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMorphologicalGradientImageFilter.txx"
#endif

#endif


