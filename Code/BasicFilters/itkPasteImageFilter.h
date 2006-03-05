/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPasteImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPasteImageFilter_h
#define __itkPasteImageFilter_h

#include "itkInPlaceImageFilter.h"
#include "itkSmartPointer.h"

namespace itk
{

/** \class PasteImageFilter
 * \brief Paste an image into another image
 *
 * PasteImageFilter allows you to take a section of one image and
 * paste into another image.  The SetDestinationIndex() method
 * prescribes where in the first input to start pasting data from the
 * second input.  The SetSourceRegion method prescribes the section of
 * the second image to paste into the first. If the output requested
 * region does not have include the SourceRegion after it has been
 * repositioned to DestinationIndex, then the output will just be
 * a copy of the input.
 *
 * The two inputs and output image will have the same pixel type.
 *
 * \ingroup GeometricTransforms
 */
template <class TInputImage, class TSourceImage=TInputImage, class TOutputImage=TInputImage>
class ITK_EXPORT PasteImageFilter:
    public InPlaceImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef PasteImageFilter         Self;
  typedef InPlaceImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(PasteImageFilter, InPlaceImageFilter);

  /** Typedefs from Superclass */
  typedef typename Superclass::InputImagePointer InputImagePointer;
  typedef typename Superclass::OutputImagePointer OutputImagePointer;

  typedef typename TSourceImage::Pointer SourceImagePointer;
  typedef typename TSourceImage::ConstPointer SourceImageConstPointer;

  /** Typedef to describe the output and input image region types. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;
  typedef typename TInputImage::RegionType InputImageRegionType;
  typedef typename TSourceImage::RegionType SourceImageRegionType;

  /** Typedef to describe the type of pixel. */
  typedef typename TOutputImage::PixelType OutputImagePixelType;
  typedef typename TInputImage::PixelType InputImagePixelType;
  typedef typename TSourceImage::PixelType SourceImagePixelType;

  /** Typedef to describe the output and input image index and size types. */
  typedef typename TOutputImage::IndexType OutputImageIndexType;
  typedef typename TOutputImage::SizeType OutputImageSizeType;
  typedef typename TInputImage::IndexType InputImageIndexType;
  typedef typename TInputImage::SizeType InputImageSizeType;
  typedef typename TSourceImage::IndexType SourceImageIndexType;
  typedef typename TSourceImage::SizeType SourceImageSizeType;

  /** ImageDimension enumeration */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(SourceImageDimension, unsigned int,
                      TSourceImage::ImageDimension);

  /** Set/Get the destination index (where in the first input the second
   * input will be pasted. */
  itkSetMacro(DestinationIndex, InputImageIndexType);
  itkGetMacro(DestinationIndex, InputImageIndexType);

  /** Set/Get the source region (what part of the second input will be
   * pasted. */
  itkSetMacro(SourceRegion, SourceImageRegionType);
  itkGetMacro(SourceRegion, SourceImageRegionType);

  /** Set/Get the "destination" image.  This is the image that will be
   * obscured by the paste operation. */
  void SetDestinationImage(TInputImage *dest) { this->SetNthInput(0, dest); }
  const TInputImage* GetDestinationImage() { return this->GetInput(0); };

  /** Set/Get the "source" image.  This is the image that will be
   * pasted over the destination image. */
  void SetSourceImage(TSourceImage *src) { this->SetNthInput(1, src); }
  const TSourceImage* GetSourceImage() { return this->GetInput(1); };
  
  /** PasteImageFilter needs to set the input requested regions for its
   * inputs.  The first input's requested region will be set to match
   * the output requested region.  The second input's requested region
   * will be set to the value of the m_SourceRegion ivar.  Note that
   * if the output requested region is a portion of the image that
   * is outside the DestinationIndex + size of the source region,
   * then the first input is copied to the output.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion();


protected:
  PasteImageFilter();
  ~PasteImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** PasteImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData().  ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

  SourceImageRegionType m_SourceRegion;
  InputImageIndexType m_DestinationIndex;

private:
  PasteImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPasteImageFilter.txx"
#endif
  
#endif
