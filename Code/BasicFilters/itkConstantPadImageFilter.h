/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstantPadImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConstantPadImageFilter_h
#define __itkConstantPadImageFilter_h

#include "itkPadImageFilter.h"

namespace itk
{

/** \class ConstantPadImageFilter
 * \brief Increase the image size by padding with a constant value.
 *
 * ConstantPadImageFilter changes the output image region.  If the output
 * image region is larger than the input image region, the extra pixels are
 * filled in by a constant value.  The output image region must be specified.
 *
 * This filter is implemented as a multithreaded filter.  It provides a 
 * ThreadedGenerateData() method for its implementation.
 * 
 * \ingroup GeometricTransforms
 * */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT ConstantPadImageFilter:
    public PadImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef ConstantPadImageFilter         Self;
  typedef PadImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self); 

  /** Run-time type information (and related methods). */
  itkTypeMacro(ConstantPadImageFilter, PadImageFilter);

  /** Typedef to describe the output image region type. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;
  typedef typename Superclass::InputImageRegionType InputImageRegionType;

  /** Typedef to describe the type of pixel. */
  typedef typename Superclass::OutputImagePixelType OutputImagePixelType;
  typedef typename Superclass::InputImagePixelType InputImagePixelType;

  /** Typedef to describe the output and input image index and size types. */
  typedef typename Superclass::OutputImageIndexType OutputImageIndexType;
  typedef typename Superclass::InputImageIndexType InputImageIndexType;
  typedef typename Superclass::OutputImageSizeType OutputImageSizeType;
  typedef typename Superclass::InputImageSizeType InputImageSizeType;

  /** ImageDimension constant */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Set/Get the pad value.  Default is Zero. */
  itkSetMacro(Constant, OutputImagePixelType);
  itkGetMacro(Constant, OutputImagePixelType);
                 
protected:
  ConstantPadImageFilter();
  ~ConstantPadImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** PadImageFilter can be implemented as a multithreaded filter.  Therefore,
   * this implementation provides a ThreadedGenerateData() routine which
   * is called for each processing thread. The output image data is allocated
   * automatically by the superclass prior to calling ThreadedGenerateData().
   * ThreadedGenerateData can only write to the portion of the output image
   * specified by the parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

  /**
   * Given an n dimensional list of output region breakpoints in indices
   * and size (where the current region and maximum region for each dimension
   * is encoded in regIndices and regLimit), choose the next output region.
   */ 
  int GenerateNextRegion(long *regIndices, long *regLimit, 
                         OutputImageIndexType *indices, 
                         OutputImageSizeType *sizes, 
                         OutputImageRegionType& outputRegion);
  
private:
  ConstantPadImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  OutputImagePixelType m_Constant;
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConstantPadImageFilter.txx"
#endif
  
#endif
