/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCropImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCropImageFilter_h
#define __itkCropImageFilter_h

#include "itkExtractImageFilter.h"

namespace itk
{

/** \class CropImageFilter
 * \brief Decrease the image size by cropping the image by an itk::Size at
 * both the upper and lower bounds of the largest possible region.
 *
 * CropImageFilter changes the image boundary of an image by removing 
 * pixels outside the target region.  The target region is not specified in
 * advance, but calculated in BeforeThreadedGenerateData().
 *
 * This filter uses ExtractImageFilter to perform the cropping.
 * 
 * \ingroup GeometricTransforms
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT CropImageFilter:
    public ExtractImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef CropImageFilter         Self;
  typedef ExtractImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(CropImageFilter, ExtractImageFilter);

  /** Typedef to describe the output and input image region types. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;
  typedef typename Superclass::InputImageRegionType InputImageRegionType;

  /** Typedef to describe the type of pixel. */
  typedef typename Superclass::OutputImagePixelType OutputImagePixelType;
  typedef typename Superclass::InputImagePixelType  InputImagePixelType;

  /** Typedef to describe the output and input image index and size types. */
  typedef typename Superclass::OutputImageIndexType OutputImageIndexType;
  typedef typename Superclass::InputImageIndexType  InputImageIndexType;
  typedef typename Superclass::OutputImageSizeType  OutputImageSizeType;
  typedef typename Superclass::InputImageSizeType   InputImageSizeType;
  typedef InputImageSizeType SizeType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      Superclass::InputImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      Superclass::OutputImageDimension);

  /** Set/Get the cropping sizes for the upper and lower boundaries. */
  itkSetMacro(UpperBoundaryCropSize, SizeType);
  itkGetMacro(UpperBoundaryCropSize, SizeType); 
  itkSetMacro(LowerBoundaryCropSize, SizeType);
  itkGetMacro(LowerBoundaryCropSize, SizeType);
  
                 
protected:
  CropImageFilter()
  {
    m_UpperBoundaryCropSize.Fill(0);
    m_LowerBoundaryCropSize.Fill(0);
  };
  ~CropImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;


  void GenerateOutputInformation();
  
private:
  CropImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  SizeType m_UpperBoundaryCropSize;
  SizeType m_LowerBoundaryCropSize;
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCropImageFilter.txx"
#endif
  
#endif
