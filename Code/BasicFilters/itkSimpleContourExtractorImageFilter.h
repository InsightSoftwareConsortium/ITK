/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimpleContourExtractorImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSimpleContourExtractorImageFilter_h
#define __itkSimpleContourExtractorImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include "itkNumericTraits.h"

namespace itk
{
/** \class SimpleContourExtractorImageFilter
* \brief Computes an image of contours from
*
* Computes an image which will be the contour of the first image. A
* pixel of the source image  is considered to belong to the contour,
* if its pixel value is equal to the input Foreground value  and it
* has in its neighborhood at least one pixel which its pixel value is
* equal to the input background  value. The output image will have
* pixels which will be set to the output foreground value if they
* belong to the contour, otherwise they will be set to the ouput
* background value. 
* The neighborhood "radius" is set thanks to the radius params.
*
* \sa Image
* \sa Neighborhood
* \sa NeighborhoodOperator
* \sa NeighborhoodIterator
* 
* \ingroup IntensityImageFilters
  */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT SimpleContourExtractorImageFilter :
    public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Extract dimension from input and output image. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
    TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
    TOutputImage::ImageDimension);
  
  /** Convenient typedefs for simplifying declarations. */
  typedef TInputImage InputImageType;
  typedef TOutputImage OutputImageType;
  
  /** Standard class typedefs. */
  typedef SimpleContourExtractorImageFilter Self;
  typedef ImageToImageFilter< InputImageType, OutputImageType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(SimpleContourExtractorImageFilter, ImageToImageFilter);
  
  /** Image typedef support. */
  typedef typename InputImageType::PixelType InputPixelType;
  typedef typename OutputImageType::PixelType OutputPixelType;
  typedef typename NumericTraits<InputPixelType>::RealType InputRealType;
  
  typedef typename InputImageType::RegionType InputImageRegionType;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  
  typedef typename InputImageType::SizeType InputSizeType;
  
  /** Set the radius of the neighborhood used to identify border
   * pixel. */
  itkSetMacro(Radius, InputSizeType);
  
  /** Get the radius of the neighborhood used to identify border
   * pixel. */
  itkGetConstReferenceMacro(Radius, InputSizeType);

  /** Set the foreground value used in order to identify a foreground
   * pixel in the input image. */
  itkSetMacro(InputForegroundValue, InputPixelType);

  /** Get the foreground value used in order to identify a foreground
   * pixel in the input image. */
  itkGetConstReferenceMacro(InputForegroundValue, InputPixelType);

  /** Set the background value used in order to identify a background
   * pixel in the input image. */
  itkSetMacro(InputBackgroundValue, InputPixelType);
  
  /** Get the background value used in order to identify a background
   * pixel in the input image. */
  itkGetConstReferenceMacro(InputBackgroundValue, InputPixelType);

  /** Set the foreground value used in order to identify a foreground
   * pixel in the output image. */
  itkSetMacro(OutputForegroundValue, OutputPixelType);

  /** Get the foreground value used in order to identify a foreground
   * pixel in the output image. */
  itkGetConstReferenceMacro(OutputForegroundValue, OutputPixelType);

  /** Set the background value used in order to identify a background
   * pixel in the output image. */
  itkSetMacro(OutputBackgroundValue, OutputPixelType);

  /** Get the background value used in order to identify a background
   * pixel in the output image. */
  itkGetConstReferenceMacro(OutputBackgroundValue, OutputPixelType);
  
  /** SimpleContourExtractorImageFilter needs a larger input requested
  * region than the output requested region.  As such,
  * SimpleContourExtractorImageFilter needs to provide an
  * implementation for GenerateInputRequestedRegion() in order to
  * inform the pipeline execution model. 
  *
  * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputHasNumericTraitsCheck,
                  (Concept::HasNumericTraits<InputPixelType>));
  itkConceptMacro(OutputHasNumericTraitsCheck,
                  (Concept::HasNumericTraits<OutputPixelType>));
  /** End concept checking */
#endif

protected:
  SimpleContourExtractorImageFilter();
  virtual ~SimpleContourExtractorImageFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** SimpleContourExtractorImageFilter can be implemented as a
     * multithreaded filter. Therefore, this implementation provides a
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
  
private:
  SimpleContourExtractorImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  InputSizeType m_Radius;
  InputPixelType m_InputForegroundValue;
  InputPixelType m_InputBackgroundValue;
  OutputPixelType m_OutputForegroundValue;
  OutputPixelType m_OutputBackgroundValue;
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimpleContourExtractorImageFilter.txx"
#endif

#endif
