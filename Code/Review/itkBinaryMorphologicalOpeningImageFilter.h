/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryMorphologicalOpeningImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkBinaryMorphologicalOpeningImageFilter_h
#define __itkBinaryMorphologicalOpeningImageFilter_h

#include "itkKernelImageFilter.h"

namespace itk
{
/**
 * \class BinaryMorphologicalOpeningImageFilter
 * \brief binary morphological closing of an image.
 *
 * This filter removes small (i.e., smaller than the structuring
 * element) structures in the interior or at the
 * boundaries of the image. The morphological opening of an image
 * "f" is defined as:
 * Opening(f) = Dilatation(Erosion(f)).
 *
 * The structuring element is assumed to be composed of binary
 * values (zero or one). Only elements of the structuring element
 * having values > 0 are candidates for affecting the center pixel.
 *
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa MorphologyImageFilter, GrayscaleDilateImageFilter, GrayscaleErodeImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */

template< class TInputImage, class TOutputImage, class TKernel >
class ITK_EXPORT BinaryMorphologicalOpeningImageFilter:
  public KernelImageFilter< TInputImage, TOutputImage, TKernel >
{
public:
  /** Standard class typedefs. */
  typedef BinaryMorphologicalOpeningImageFilter                   Self;
  typedef KernelImageFilter< TInputImage, TOutputImage, TKernel > Superclass;
  typedef SmartPointer< Self >                                    Pointer;
  typedef SmartPointer< const Self >                              ConstPointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BinaryMorphologicalOpeningImageFilter,
               KernelImageFilter);

  typedef TInputImage                          InputImageType;
  typedef TOutputImage                         OutputImageType;
  typedef typename InputImageType::Pointer     InputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType;

  /** Declaration of pixel type. */
  typedef typename TInputImage::PixelType PixelType;

  /** Kernel typedef. */
  typedef TKernel KernelType;

  /** Set the value in the image to consider as "foreground". Defaults to
   * maximum value of PixelType. */
  itkSetMacro(ForegroundValue, PixelType);

  /** Get the value in the image considered as "foreground". Defaults to
   * maximum value of PixelType. */
  itkGetConstMacro(ForegroundValue, PixelType);

  /** Set the value in eroded part of the image. Defaults to zero */
  itkSetMacro(BackgroundValue, PixelType);

  /** Set the value in eroded part of the image. Defaults to zero */
  itkGetConstMacro(BackgroundValue, PixelType);
protected:
  BinaryMorphologicalOpeningImageFilter();
  ~BinaryMorphologicalOpeningImageFilter() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleDilateImageFilter GrayscaleErodeImageFilter. */
  void  GenerateData();

private:
  BinaryMorphologicalOpeningImageFilter(const Self &); //purposely not
                                                       // implemented
  void operator=(const Self &);                        //purposely not
                                                       // implemented

  PixelType m_ForegroundValue;

  PixelType m_BackgroundValue;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryMorphologicalOpeningImageFilter.txx"
#endif

#endif
