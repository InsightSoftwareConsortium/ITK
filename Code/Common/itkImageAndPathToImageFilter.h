/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageAndPathToImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageAndPathToImageFilter_h
#define __itkImageAndPathToImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
  
/** \class ImageAndPathToImageFilter
 * \brief Base class for filters that take both a path and an image as input and produce a path as output.
 *
 * This class is the base class for filters that take both an image and a path
 * as input and produce an image as output.  Specifically, this class defines
 * the methods SetPathInput() and SetImageInput().  (It also establishes the
 * precedent of having image inputs preceed path inputs for functions producing
 * images as outputs, according to the underlying DataObject implementation.)
 * 
 * \ingroup ImageFilters
 * \ingroup PathFilters
 */
template <class TInputImage, class TInputPath, class TOutputImage>
class ITK_EXPORT ImageAndPathToImageFilter :
    public ImageToImageFilter<TInputImage,TOutputImage> 
{
public:
  /** Standard class typedefs. */
  typedef ImageAndPathToImageFilter  Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageAndPathToImageFilter, ImageToImageFilter);

  /** Some convenient typedefs. */
  typedef          TInputImage                    InputImageType;
  typedef typename InputImageType::ConstPointer   InputImagePointer;
  typedef typename InputImageType::RegionType     InputImageRegionType; 
  typedef typename InputImageType::PixelType      InputImagePixelType; 
  typedef          TInputPath                     InputPathType;
  typedef typename InputPathType::Pointer         InputPathPointer;
  typedef typename InputPathType::ConstPointer    InputPathConstPointer;
  typedef typename InputPathType::InputType       InputPathInputType;
  typedef typename InputPathType::OutputType      InputPathOutputType;
  typedef typename InputPathType::IndexType       InputPathIndexType;
  typedef typename InputPathType::OffsetType      InputPathOffsetType;
  typedef          TOutputImage                   OutputImageType;
  typedef typename OutputImageType::Pointer       OutputImagePointer;
  typedef typename OutputImageType::RegionType    OutputImageRegionType; 
  typedef typename OutputImageType::PixelType     OutputImagePixelType; 

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  
  /** Set/Get the image input of this process object. */
  virtual void SetImageInput( const TInputImage * image);
  const InputImageType * GetImageInput(void);
  
  /** Set/Get the path input of this process object. */
  virtual void SetPathInput( const TInputPath * path);
  const InputPathType * GetPathInput(void);

protected:
  ImageAndPathToImageFilter();
  virtual ~ImageAndPathToImageFilter() {};

  virtual void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  ImageAndPathToImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageAndPathToImageFilter.txx"
#endif

#endif
