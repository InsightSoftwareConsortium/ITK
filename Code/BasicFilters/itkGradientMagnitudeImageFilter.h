/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientMagnitudeImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGradientMagnitudeImageFilter_h
#define __itkGradientMagnitudeImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"

namespace itk
{
/** \class GradientMagnitudeImageFilter
 * \brief Computes the gradient magnitude of an image region at each pixel.
 *
 * 
 * \ingroup GradientFilters
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 */
template <typename TInputImage, typename TOutputImage>
class ITK_EXPORT GradientMagnitudeImageFilter :
    public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef GradientMagnitudeImageFilter Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(GradientMagnitudeImageFilter, ImageToImageFilter);
  
  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TInputImage::PixelType InputPixelType;
  typedef typename NumericTraits<InputPixelType>::RealType RealType;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  
  /** Image typedef support */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;
  typedef typename InputImageType::Pointer InputImagePointer;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  
  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  /** GradientMagnitudeImageFilter needs a larger input requested
   * region than the output requested region (larger by the kernel
   * size to calculate derivatives).  As such,
   * GradientMagnitudeImageFilter needs to provide an implementation
   * for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

  /** Use the image spacing information in calculations. Use this option if you
   *  want derivatives in physical space. Default is UseImageSpacingOn. */
  void SetUseImageSpacingOn()
  { this->SetUseImageSpacing(true); }
  
  /** Ignore the image spacing. Use this option if you want derivatives in
      isotropic pixel space.  Default is UseImageSpacingOn. */
  void SetUseImageSpacingOff()
  { this->SetUseImageSpacing(false); }
  
  /** Set/Get whether or not the filter will use the spacing of the input
      image in its calculations */
  itkSetMacro(UseImageSpacing, bool);
  itkGetMacro(UseImageSpacing, bool);

protected:
  GradientMagnitudeImageFilter()
  {
    m_UseImageSpacing = true;
  }
  virtual ~GradientMagnitudeImageFilter() {}

  /** GradientMagnitudeImageFilter can be implemented as a
   * multithreaded filter.  Therefore, this implementation provides a
   * ThreadedGenerateData() routine which is called for each
   * processing thread. The output image data is allocated
   * automatically by the superclass prior to calling
   * ThreadedGenerateData().  ThreadedGenerateData can only write to
   * the portion of the output image specified by the parameter
   * "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

  void PrintSelf(std::ostream& os, Indent indent) const
  {
    Superclass::PrintSelf(os,indent);
    os << indent << "UseImageSpacing = " << m_UseImageSpacing << std::endl;
  }
  
private:
  GradientMagnitudeImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  bool m_UseImageSpacing;

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientMagnitudeImageFilter.txx"
#endif

#endif
