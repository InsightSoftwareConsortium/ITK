/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWrapPadImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWrapPadImageFilter_h
#define __itkWrapPadImageFilter_h

#include "itkPadImageFilter.h"
#include <vector>

namespace itk
{

/** \class WrapPadImageFilter
 * \brief Increase the image size by padding with replicants of the 
 * input image value.
 *
 * WrapPadImageFilter changes the image bounds of an image.  Added pixels are 
 * filled in with a wrapped replica of the input image.  The image bounds 
 * of the output must be specified.
 *
 * This filter is implemented as a multithreaded filter.  It provides a 
 * ThreadedGenerateData() method for its implementation.
 * 
 * \ingroup GeometricTransforms
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT WrapPadImageFilter:
    public PadImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef WrapPadImageFilter         Self;
  typedef PadImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self); 
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(WrapPadImageFilter, PadImageFilter);

  typedef typename Superclass::InputImageType InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename Superclass::InputImagePointer InputImagePointer;
  typedef typename Superclass::OutputImagePointer OutputImagePointer;
  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;
  
  /** Typedef to describe the output image region type. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;
  typedef typename Superclass::InputImageRegionType InputImageRegionType;
  
  /** Typedef to describe the type of pixel. */
  typedef typename Superclass::OutputImagePixelType OutputImagePixelType;
  typedef typename Superclass::InputImagePixelType InputImagePixelType;
  
  /** Typedef to describe the output and input image index and size types. */
  typedef typename Superclass::OutputImageIndexType OutputImageIndexType;
  typedef typename Superclass::InputImageIndexType InputImageIndexType;
  typedef typename Superclass::OutputImageSizeType OutputImageSizeType;
  typedef typename Superclass::InputImageSizeType InputImageSizeType;
  
  /** ImageDimension enumeration. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension );
  
protected:
  WrapPadImageFilter() {};
  ~WrapPadImageFilter() {};
  
  /** WrapPadImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData().  ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

  /** Given an n dimensional list of input region breakpoints in indices
   * and size (where the current region and maximum region for each dimension
   * is encoded in regIndices and regLimit), choose the next input region. */ 
  int GenerateNextInputRegion(long *regIndices, long *regLimit, 
            std::vector<long> indices[], 
            std::vector<long> sizes[], 
            InputImageRegionType& outputRegion);

  /** Given an n dimensional list of output region breakpoints in indices
   * and size (where the current region and maximum region for each dimension
   * is encoded in regIndices and regLimit), choose the next output region. */ 
  int GenerateNextOutputRegion(long *regIndices, long *regLimit, 
             std::vector<long> indices[], 
             std::vector<long> sizes[], 
             OutputImageRegionType& outputRegion);

  /** Given the start and end indices of a region, determine how many
   * instances of size fit within the region.  The variable offset provides
   * a way to adjust width of the area while forcing alignment to the
   * start or end location. */
  int FindRegionsInArea(long start, long end, long size, long offset);

  /** Generate region 0 (inter-region) information.  Based on the indices
   * of the input and the output for this dimension, decide what are the 
   * starting points and the lengths of the output region directly 
   * corresponding to the input region.  Padding will be on either 
   * side of this region.  The algorithmic complications are necessary
   * to support the streaming interface and multithreading. */
  int BuildInterRegions(std::vector<long>& inputRegionStart, 
                        std::vector<long>& outputRegionStart,
                        std::vector<long>& inputRegionSizes, 
                        std::vector<long>& outputRegionSizes,
                        long inputIndex, long outputIndex,
                        long inputSize, long outputSize, int numRegs, 
                        int & regCtr);

  /** Generate region 1 (pre-region) information.  Based on the indices
   * of the input and the output for this dimension, decide what are the 
   * starting points and the lengths of the output region directly 
   * preceding the input region in this dimension.  This may require
   * more than one region be defined if the padding is larger than the
   * size of the input image in this dimension.  Other algorithmic 
   * complications are necessary to support the streaming interface 
   * and multithreading. */
  int BuildPreRegions(std::vector<long>& inputRegionStart, 
                      std::vector<long>& outputRegionStart,
                      std::vector<long>& inputRegionSizes, 
                      std::vector<long>& outputRegionSizes,
                      long inputIndex, long outputIndex,
                      long inputSize, long outputSize, int numRegs, 
                      int & regCtr);

  /** Generate region 2 (post-region) information.  Based on the indices
   * of the input and the output for this dimension, decide what are the 
   * starting points and the lengths of the output region directly 
   * succeeding the input region in this dimension.  This may require
   * more than one region be defined if the padding is larger than the
   * size of the input image in this dimension.  Other algorithmic 
   * complications are necessary to support the streaming interface 
   * and multithreading. */
  int BuildPostRegions(std::vector<long>& inputRegionStart, 
                       std::vector<long>& outputRegionStart,
                       std::vector<long>& inputRegionSizes, 
                       std::vector<long>& outputRegionSizes,
                       long inputIndex, long outputIndex,
                       long inputSize, long outputSize, 
                       int numRegs, int & regCtr);

  /** WrapPadImageFilter needs a different input requested region than
   * output requested region.  As such, WrapPadImageFilter needs to
   * provide an implementation for GenerateInputRequestedRegion() in
   * order to inform the pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() 
   * \sa PadImageFilter::GenerateInputRequestedRegion()  */
  virtual void GenerateInputRequestedRegion();

private:
  WrapPadImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};
  

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWrapPadImageFilter.txx"
#endif

#endif
