/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShrinkImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkShrinkImage_h
#define __itkShrinkImage_h

#include "itkFilterImageToImage.h"

namespace itk
{

/** \class ShrinkImage
 * \brief Reduce the size of an image by an integer factor.
 *
 * ShrinkImage reduces the size of an image by an integer factor. The
 * algorithm implemented is a simple subsample. Since this filter produces
 * an image which is a different resolution and with different pixel spacing
 * than its input image, it needs to override several of the methods defined
 * in ProcessObject in order to properly manage the pipeline execution model.
 * In particular, this filter overrides
 * ProcessObject::GenerateInputRequestedRegion() and
 * ProcessObject::UpdateOutputInformation().
 *
 * This filter is implemented as a multithreaded filter.  It provides a 
 * ThreadedGenerateData() method for its implementation.
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT ShrinkImage:
    public FilterImageToImage<TInputImage,TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ShrinkImage         Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef FilterImageToImage<TInputImage,TOutputImage>  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  

  /**
   * Typedef to describe the output image region type.
   */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ShrinkImage, FilterImageToImage);

  /** 
   * Set the shrink factor. The default value is 1.
   */
  itkSetClampMacro(ShrinkFactor,int, 1,
                   NumericTraits<int>::max());
  
  /** 
   * Get the shrink factor.
   */
  itkGetMacro(ShrinkFactor,int);
                 
  /**
   * ShrinkImage produces an image which is a different resolution and
   * with a different pixel spacing than its input image.  As such,
   * ShrinkImage needs to provide an implementation for
   * UpdateOutputInformation() in order to inform the pipeline execution model.
   * The original documentation of this method is below.
   *
   * \sa ProcessObject::UpdateOutputInformaton()
   */
  virtual void UpdateOutputInformation();

  /**
   * ShrinkImage needs a larger input requested region than the output
   * requested region.  As such, ShrinkImage needs to provide an implementation
   * for GenerateInputRequestedRegion() in order to inform the pipeline
   * execution model.  
   *
   * \sa ProcessObject::GenerateInputRequestedRegion()
   */
  virtual void GenerateInputRequestedRegion();

 protected:
  ShrinkImage();
  ~ShrinkImage() {};
  ShrinkImage(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);

  /**
   * ShrinkImage can be implemented as a multithreaded filter.  Therefore,
   * this implementation provides a ThreadedGenerateData() routine which
   * is called for each processing thread. The output image data is allocated
   * automatically by the superclass prior to calling ThreadedGenerateData().
   * ThreadedGenerateData can only write to the portion of the output image
   * specified by the parameter "outputRegionForThread"
   *
   * \sa FilterImageToImage::ThreadedGenerateData(),
   *     FilterImageToImage::GenerateData()
   */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

private:
  int m_ShrinkFactor;
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShrinkImage.txx"
#endif
  
#endif
