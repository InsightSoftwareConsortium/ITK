/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMinimumMaximumImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMinimumMaximumImageFilter_h
#define __itkMinimumMaximumImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
/** \class MinimumMaximumImageFilter
 * \brief Computes the minimum and the maximum intensity values of
 * an image. 
 *
 * It is templated over input image type only.
 * This filter just copy the input image through this output to
 * be included within the pipeline.
 *
 * \ingroup Operators
 * \todo Use itkImageToValueFilter when available
 */
template <class TInputImage>
class ITK_EXPORT MinimumMaximumImageFilter :
    public ImageToImageFilter< TInputImage, TInputImage>
{
public:
  /** Extract dimension from input image. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Standard class typedefs. */
  typedef MinimumMaximumImageFilter Self;
  typedef ImageToImageFilter< TInputImage, TInputImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MinimumMaximumImageFilter, ImageToImageFilter);
  
  /** Image typedef support. */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::PixelType InputPixelType;

  /** Return the minimum intensity value. */
  itkGetMacro(Minimum,InputPixelType);
  
  /** Return the maximum intensity value. */
  itkGetMacro(Maximum,InputPixelType);

protected:
  MinimumMaximumImageFilter()
  {
    m_Minimum = NumericTraits<InputPixelType>::NonpositiveMin();
    m_Maximum = NumericTraits<InputPixelType>::max();
  }
  virtual ~MinimumMaximumImageFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateData();

private:
  MinimumMaximumImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  InputPixelType    m_Minimum;
  InputPixelType    m_Maximum;
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMinimumMaximumImageFilter.txx"
#endif

#endif
