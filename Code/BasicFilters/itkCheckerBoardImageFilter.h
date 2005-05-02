/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCheckerBoardImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCheckerBoardImageFilter_h
#define __itkCheckerBoardImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{

/** \class CheckerBoardImageFilter
 * \brief Combines two images in a checkerboard pattern.
 *
 * CheckerBoardImageFilter takes two input images that must have the same
 * dimension, size, origin and spacing and produces an output image of the same
 * size by combinining the pixels from the two input images in a checkerboard
 * pattern. This filter is commonly used for visually comparing two images, in
 * particular for evaluating the results of an image registration process.
 * 
 * This filter is implemented as a multithreaded filter.  It provides a 
 * ThreadedGenerateData() method for its implementation.
 *
 * \ingroup IntensityImageFilters  Multithreaded
 */
template <class TImage>
class ITK_EXPORT CheckerBoardImageFilter:
    public ImageToImageFilter<TImage, TImage>
{
public:
  /** Standard class typedefs. */
  typedef CheckerBoardImageFilter            Self;
  typedef ImageToImageFilter<TImage,TImage>  Superclass;
  typedef SmartPointer<Self>                 Pointer;
  typedef SmartPointer<const Self>           ConstPointer;
  typedef TImage InputImageType;
  typedef TImage OutputImageType;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename OutputImageType::Pointer     OutputImagePointer;
  typedef typename OutputImageType::RegionType  ImageRegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(CheckerBoardImageFilter, ImageToImageFilter);

  /** Number of dimensions. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension);

  /** Type to hold the number of checker boxes per dimension */
  typedef itk::FixedArray< unsigned int, ImageDimension >  PatternArrayType;
  
  /** Connect one of the operands for checker board */
  void SetInput1( const TImage * image1);

  /** Connect one of the operands for checker board */
  void SetInput2( const TImage * image2);


protected:
  CheckerBoardImageFilter();
  ~CheckerBoardImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** CheckerBoardImageFilter can be implemented as a multithreaded filter.  Therefore,
   * this implementation provides a ThreadedGenerateData() routine which
   * is called for each processing thread. The output image data is allocated
   * automatically by the superclass prior to calling ThreadedGenerateData().
   * ThreadedGenerateData can only write to the portion of the output image
   * specified by the parameter "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void ThreadedGenerateData(const ImageRegionType& outputRegionForThread,
                            int threadId );

private:
  CheckerBoardImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  PatternArrayType  m_CheckerPattern;
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCheckerBoardImageFilter.txx"
#endif
  
#endif
