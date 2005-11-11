/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryDilateImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryDilateImageFilter_h
#define __itkBinaryDilateImageFilter_h

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
 * \class BinaryDilateImageFilter
 * \brief Fast binary dilation
 *
 * BinaryDilateImageFilter is a binary dilation
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
 * "DilateValue".  Pixel values matching the dilate value are
 * considered the "foreground" and all other pixels are
 * "background". This is useful in processing segmented images where
 * all pixels in segment #1 have value 1 and pixels in segment #2 have
 * value 2, etc. A particular "segment number" can be processed.
 * DilateValue defaults to the maximum possible value of the
 * PixelType.
 *
 * The structuring element is assumed to be composed of binary values
 * (zero or one). Only elements of the structuring element having
 * values > 0 are candidates for affecting the center pixel.  A
 * reasonable choice of structuring element is
 * itk::BinaryBallStructuringElement.
 *
 *
 * Description of the algorithm:
 * ----------------------------------------------
 * Let's consider the set of the ON elements of the input image as X.
 *
 * Let's consider the structuring element as B = {B0, B1, ..., Bn},
 * where Bi denotes a connected component of B.
 *
 * Let's consider bi, i in [0,n], an arbitrary point of Bi.
 *
 * We use hence the next property in order to compute minkoswki
 * addition ( which will be written (+) ):
 *
 * X (+) B = ( Xb0 UNION Xb1 UNION ... Xbn ) UNION ( BORDER(X) (+) B ),
 *
 * where Xbi is the set X translated with respect to vector bi :
 *
 * Xbi ={ x + bi, x belongs to X }
 *
 * where BORDER(X) is the extracted border of X ( 8 connectivity in
 * 2D, 26 in 3D ) 
 *
 * Our implementation for dilation is defined as:
 *
 *     X (+) SYM(B) = DILATION(X)_B
 *
 * Where DILATION(X)_B is the dilation of set with structuring element B.
 * Where SYM(B) is the symmetric of the structuring element relatively
 * to its center. 
 *
 * This class was contributed by Jerome Schmid from the University of
 * Strasbourg.
 *
 * \todo Implement a threaded version ?
 *
 * \sa ImageToImageFilter
 * \sa BinaryErodeImageFilter
 */
template <class TInputImage, class TOutputImage, class TKernel>
class ITK_EXPORT BinaryDilateImageFilter :
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
  typedef BinaryDilateImageFilter Self;
  typedef BinaryMorphologyImageFilter<InputImageType, OutputImageType, KernelType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryDilateImageFilter, BinaryMorphologyImageFilter);

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
  void SetDilateValue(const InputPixelType& value)
    { this->SetForegroundValue( value ); }

  /** Get the value in the image considered as "foreground". Defaults to
   * maximum value of PixelType. This is an alias to the
   * ForegroundValue in the superclass. */
  InputPixelType GetDilateValue() const
    { return this->GetForegroundValue(); }

protected:
  BinaryDilateImageFilter();
  virtual ~BinaryDilateImageFilter(){}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** BinaryDilateImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData().  ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );
  
  void BeforeThreadedGenerateData();

  // type inherited from the superclass
  typedef typename Superclass::NeighborIndexContainer NeighborIndexContainer;
  typedef typename Superclass::BorderCellContainer BorderCellContainer;
  typedef typename Superclass::BorderCell BorderCell;

private:
  BinaryDilateImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryDilateImageFilter.txx"
#endif

#endif
