/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRemoveDimensionsImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRemoveDimensionsImageFilter_h
#define __itkRemoveDimensionsImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkSmartPointer.h"

namespace itk
{

/** \class RemoveDimensionsImageFilter
 * \brief Decrease the number of dimensions in an image. 
 *
 * RemoveDimensionsImageFilter removes M number of dimensions in a image of
 * N dimensions.  The output image will have N-M dimensions (OutputImageDimension).
 * An array of unsigned int is passed in containing the dimensions to remove 
 * with the first dimension defined as 0, the second dimension as 1,...  
 * An itkIndex specifies what dimensions to remove.  The dimensions that are 
 * collapsed are specified by non-zero values in this itkIndex.  This non-zero
 * value also specifies what location in that dimension to keep.  For example, 
 * the itkIndex object containing [0,0,2,0] collapses the third dimension from a 
 * 4-D image of size [4,5,6,7] to a 3-D image of size [4,5,7], using the 2nd 
 * volume of the third dimension
 *
 * This filter is implemented as a non-threaded filter.  It provides a 
 * GenerateData() method for its implementation.
 * 
 * \ingroup GeometricTransforms
 */
template <class TInputImage, unsigned int OutputImageDimension>
class ITK_EXPORT RemoveDimensionsImageFilter:
  public ImageToImageFilter<TInputImage, 
                            Image< typename TInputImage::PixelType, OutputImageDimension> >
{
public:
  /** Standard class typedefs. */
  typedef RemoveDimensionsImageFilter Self;
  typedef ImageToImageFilter<TInputImage, 
                             Image<typename TInputImage::PixelType, OutputImageDimension> >  Superclass;

  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(RemoveDimensionsImageFilter, ImageToImageFilter);

  /** typedef the output image type */
  typedef Image< typename TInputImage::PixelType, OutputImageDimension>  TOutputImage;


  /** Typedef to describe the output and input image region types. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;
  typedef typename TInputImage::RegionType InputImageRegionType;

  /** Typedef to describe the type of pixel. */
  typedef typename TOutputImage::PixelType OutputImagePixelType;
  typedef typename TInputImage::PixelType InputImagePixelType;

  /** Typedef to describe the output and input image index and size types. */
  typedef typename TOutputImage::IndexType OutputImageIndexType;
  typedef typename TInputImage::IndexType InputImageIndexType;
  typedef typename TOutputImage::SizeType OutputImageSizeType;
  typedef typename TInputImage::SizeType InputImageSizeType;

  /** ImageDimension enumeration */
  enum { InputImageDimension = TInputImage::ImageDimension };

  /** Set/Get the output image region. */
  itkSetMacro(RemoveDimensionsIndex, InputImageIndexType);
  itkGetMacro(RemoveDimensionsIndex, InputImageIndexType);
                 
  /** RemoveDimensionsImageFilter produces an image with different number of 
   * dimension than the input image.  As such, RemoveDimensionsImageFilter 
   * needs to provide an implementation for GenerateOutputInformation() in 
   * order to inform the pipeline execution model.  The original documentation 
   * of this method is below.
   * \sa ProcessObject::GenerateOutputInformaton() */
  virtual void GenerateOutputInformation();

  /** RemoveDimensionsImageFilter needs different input requested region than 
   * the output requested region.  As such, RemoveDimensionsImageFilter needs 
   * to provide an implementation for GenerateInputRequestedRegion() in order 
   * to inform the pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion();

protected:
  RemoveDimensionsImageFilter();
  ~RemoveDimensionsImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;


  void GenerateData();

private:
  RemoveDimensionsImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  InputImageIndexType m_RemoveDimensionsIndex;
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRemoveDimensionsImageFilter.txx"
#endif
  
#endif
