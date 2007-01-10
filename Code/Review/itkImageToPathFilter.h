/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToPathFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageToPathFilter_h
#define __itkImageToPathFilter_h

#include "itkImage.h"
#include "itkPathSource.h"

namespace itk
{
  
/** \class ImageToPathFilter
 * \brief Base class for filters that take an image as input and produce an path as output.
 *
 * ImageToPathFilter is the base class for all process objects that output
 * path data and require image data as input. Specifically, this class
 * defines the SetInput() method for defining the input to a filter.
 *
 * \ingroup ImageFilters
 */
template <class TInputImage, class TOutputPath>
class ITK_EXPORT ImageToPathFilter : public PathSource<TOutputPath>
{
public:
  /** Standard class typedefs. */
  typedef ImageToPathFilter  Self;
  typedef PathSource<TOutputPath>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToPathFilter,PathSource);

  
  /** Some convenient typedefs. */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::Pointer        InputImagePointer;
  typedef typename InputImageType::ConstPointer   InputImageConstPointer;
  typedef typename InputImageType::RegionType     InputImageRegionType; 
  typedef typename InputImageType::PixelType      InputImagePixelType; 
  
  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Set/Get the image input of this process object.  */
  virtual void SetInput( const InputImageType *image);
  virtual void SetInput( unsigned int, const TInputImage * image);
  const InputImageType * GetInput(void);
  const InputImageType * GetInput(unsigned int idx);


 protected:
  ImageToPathFilter();
  ~ImageToPathFilter();

  virtual void PrintSelf(std::ostream& os, Indent indent) const;

private:
  ImageToPathFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToPathFilter.txx"
#endif

#endif
