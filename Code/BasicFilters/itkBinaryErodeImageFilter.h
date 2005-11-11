/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryErodeImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryErodeImageFilter_h
#define __itkBinaryErodeImageFilter_h

#include <vector>
#include <queue>
#include "itkBinaryMorphologyImageFilter.h"
#include "itkImage.h"
#include "itkNumericTraits.h"
#include "itkNeighborhoodIterator.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhood.h"
#include "itkImageBoundaryCondition.h"
#include "itkImageRegionIterator.h"
#include "itkConceptChecking.h"

namespace itk
{
/**
 * \class BinaryErodeImageFilter
 * \brief Fast binary erosion
 *
 * BinaryErodeImageFilter is a binary erosion
 * morphologic operation. This implementation is based on the papers:
 *
 * L.Vincent "Morphological transformations of binary images with
 * arbitrary structuring elements", and
 *
 * N.Nikopoulos et al. "An efficient algorithm for 3d binary
 * morphological transformations with 3d structuring elements 
 * for arbitrary size and shape". IEEE Transactions on Image
 * Processing. Vol. 9. No. 3. 2000. pp. 283-286.
 *
 * Gray scale images can be processed as binary images by selecting a
 * "ErodeValue".  Pixel values matching the dilate value are
 * considered the "foreground" and all other pixels are
 * "background". This is useful in processing segmented images where
 * all pixels in segment #1 have value 1 and pixels in segment #2 have
 * value 2, etc. A particular "segment number" can be processed.
 * ErodeValue defaults to the maximum possible value of the
 * PixelType.
 *
 * The structuring element is assumed to be composed of binary values
 * (zero or one). Only elements of the structuring element having
 * values > 0 are candidates for affecting the center pixel.  A
 * reasonable choice of structuring element is
 * itk::BinaryBallStructuringElement.
 *
 * This class was contributed by Gaetan Lehmann from INRA de Jouy-en-Josas.
 *
 * \todo Implement a threaded version ?
 *
 * \sa ImageToImageFilter
 * \sa BinaryDilateImageFilter
 */
template <class TInputImage, class TOutputImage, class TKernel>
class ITK_EXPORT BinaryErodeImageFilter :
    public BinaryMorphologyImageFilter< TInputImage, TOutputImage, TKernel >
{
public:
  /** Extract dimension from input and output image. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
        TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
        TOutputImage::ImageDimension);

  /** Extract the dimension of the kernel */
  itkStaticConstMacro(KernelDimension, unsigned int,
        TKernel::NeighborhoodDimension);
  
  /** Convenient typedefs for simplifying declarations. */
  typedef TInputImage InputImageType;
  typedef TOutputImage OutputImageType;
  typedef TKernel KernelType;
  

  /** Standard class typedefs. */
  typedef BinaryErodeImageFilter Self;
  typedef BinaryMorphologyImageFilter< InputImageType, OutputImageType, KernelType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryErodeImageFilter, BinaryMorphologyImageFilter);

  /** Kernel (structuring element) iterator. */
  typedef typename KernelType::ConstIterator KernelIteratorType ;

  /** Image typedef support. */
  typedef typename InputImageType::PixelType InputPixelType;
  typedef typename OutputImageType::PixelType OutputPixelType;
  typedef typename NumericTraits<InputPixelType>::RealType InputRealType;
  typedef typename InputImageType::OffsetType OffsetType;
  typedef typename InputImageType::IndexType IndexType;

  typedef typename InputImageType::RegionType InputImageRegionType;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  typedef typename InputImageType::SizeType InputSizeType;

  /** Set the value in the image to consider as "foreground". Defaults to
   * maximum value of PixelType. This is an alias to the
   * ForegroundValue in the superclass. */
  void SetErodeValue(const InputPixelType& value)
    { this->SetForegroundValue( value ); }

  /** Get the value in the image considered as "foreground". Defaults to
   * maximum value of PixelType. This is an alias to the
   * ForegroundValue in the superclass. */
  InputPixelType GetErodeValue() const
    { return this->GetForegroundValue(); }

protected:
  BinaryErodeImageFilter();
  virtual ~BinaryErodeImageFilter(){}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** BinaryErodeImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData().  ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread, int threadId );
    
  void BeforeThreadedGenerateData();

  // type inherited from the superclass
  typedef typename Superclass::NeighborIndexContainer NeighborIndexContainer;
  typedef typename Superclass::BorderCellContainer BorderCellContainer;
  typedef typename Superclass::BorderCell BorderCell;
  
private:
  BinaryErodeImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryErodeImageFilter.txx"
#endif

#endif
