/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExtractImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkExtractImageFilter_h
#define __itkExtractImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkSmartPointer.h"
#include "itkExtractImageFilterRegionCopier.h"

namespace itk
{

/** \class ExtractImageFilter
 * \brief Decrease the image size by cropping the image to the selected 
 * region bounds.
 *
 * ExtractImageFilter changes the image boundary of an image by removing  
 * pixels outside the target region.  The target region must be specified.
 *
 * ExtractImageFilter also collapses dimensions so that the input image 
 * may have more dimensions than the output image (i.e. 4-D input image
 * to a 3-D output image).  To specify what dimensions to collapse,
 * the ExtractionRegion must be specified.  For any dimension dim where
 * ExtractionRegion.Size[dim] = 0, that dimension is collapsed.  The 
 * index to collapse on is specified by ExtractionRegion.Index[dim].
 * For example, we have a image 4D = a 4x4x4x4 image, and we want 
 * to get a 3D image, 3D = a 4x4x4 image, specified as [x,y,z,2] from 4D 
 * (i.e. the 3rd "time" slice from 4D).  The ExtractionRegion.Size = 
 * [4,4,4,0] and ExtractionRegion.Index = [0,0,0,2].  
 *
 * The number of dimension in ExtractionRegion.Size and Index must = 
 * InputImageDimension.  The number of non-zero dimensions in 
 * ExtractionRegion.Size must = OutputImageDimension.
 *
 * This filter is implemented as a multithreaded filter.  It provides a 
 * ThreadedGenerateData() method for its implementation.
 * 
 * \ingroup GeometricTransforms
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT ExtractImageFilter:
    public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef ExtractImageFilter         Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(ExtractImageFilter, ImageToImageFilter);

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
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  typedef 
    ImageToImageFilterDetail::ExtractImageFilterRegionCopier<itkGetStaticConstMacro(InputImageDimension), 
                                                             itkGetStaticConstMacro(OutputImageDimension)> ExtractImageFilterRegionCopierType;

  /** Set/Get the output image region. 
   *  If any of the ExtractionRegion.Size = 0 for any particular dimension dim,
   *  we have to collapse dimension dim.  This means the output image will have
   *  'c' dimensions less than the input image, where c = # of 
   *  ExtractionRegion.Size = 0. */
  void SetExtractionRegion(InputImageRegionType extractRegion);
  itkGetMacro(ExtractionRegion, InputImageRegionType);

protected:
  ExtractImageFilter();
  ~ExtractImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** ExtractImageFilter can produce an image which is a different
   * resolution than its input image.  As such, ExtractImageFilter
   * needs to provide an implementation for
   * GenerateOutputInformation() in order to inform the pipeline
   * execution model.  The original documentation of this method is
   * below.
   *
   * \sa ProcessObject::GenerateOutputInformaton()  */
  virtual void GenerateOutputInformation();

  /** This function calls the actual region copier to do the mapping from
   * output image space to input image space.  It uses a 
   * Function object used for dispatching to various routines to
   * copy an output region (start index and size) to an input region.
   * For most filters, this is a trivial copy because most filters
   * require the input dimension to match the output dimension.
   * However, some filters like itk::ExtractImageFilter can
   * support output images of a lower dimension that the input.
   *
   * \sa ImageToImageFilter::CallCopyRegion() */
  virtual void CallCopyOutputRegionToInputRegion(InputImageRegionType &destRegion,
                              const OutputImageRegionType &srcRegion);

  /** ExtractImageFilter can be implemented as a multithreaded filter.
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
  InputImageRegionType m_ExtractionRegion;
  OutputImageRegionType m_OutputImageRegion;

private:
  ExtractImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkExtractImageFilter.txx"
#endif
  
#endif
