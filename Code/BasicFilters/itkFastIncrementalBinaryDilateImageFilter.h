/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastIncrementalBinaryDilateImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFastIncrementalBinaryDilateImageFilter_h
#define __itkFastIncrementalBinaryDilateImageFilter_h

#include <vector>
#include <queue>
#include "itkImageToImageFilter.h"
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
 * \class FastIncrementalBinaryDilateImageFilter
 * \brief Fast binary dilation
 *
 * FastIncrementalBinaryDilateImageFilter is a binary dilation
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
 * \sa BinaryDilateImageFilter
 */
template <class TInputImage, class TOutputImage, class TKernel>
class ITK_EXPORT FastIncrementalBinaryDilateImageFilter :
    public ImageToImageFilter< TInputImage, TOutputImage >
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

  /** Standard class typedefs. */
  typedef FastIncrementalBinaryDilateImageFilter Self;
  typedef ImageToImageFilter< InputImageType, OutputImageType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastIncrementalBinaryDilateImageFilter, ImageToImageFilter);

  /** Kernel typedef. */
  typedef TKernel KernelType;

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

  /** Input and output images must be the same dimension. */
  itkConceptMacro(ImageDimensionCheck,
      (Concept::SameDimension<itkGetStaticConstMacro(InputImageDimension),
                              itkGetStaticConstMacro(OutputImageDimension)>));

// Cannot get this to work with gcc compiler
#if 0
  /** Input and structuring element must be the same dimnesion. */
  itkConceptMacro(KernelDimensionCheck,
      (Concept::SameDimension<itkGetStaticConstMacro(KernelDimension),
                              itkGetStaticConstMacro(InputImageDimension)>));
#endif

  /** Set kernel (structuring element).*/
  void SetKernel( const KernelType& kernel );

  /** Get the kernel (structuring element). */
  itkGetConstReferenceMacro(Kernel, KernelType);

  /** Set the value in the image to consider as "foreground". Defaults to
   * maximum value of PixelType. */
  itkSetMacro(DilateValue, InputPixelType);

  /** Get the value in the image considered as "foreground". Defaults to
   * maximum value of PixelType. */
  itkGetMacro(DilateValue, InputPixelType);

  /** Set the value used as "background".  Any pixel value which is
   * not DilateValue is considered background. BackgroundValue is used
   * for defining boundary conditions. Defaults to
   * NumericTraits<PixelType>::NonpositiveMin(). */
  itkSetMacro(BackgroundValue, OutputPixelType);

  /** Get the value used as "background". Any pixel value which is
   * not DilateValue is considered background. BackgroundValue is used
   * for defining boundary conditions. Defaults to
   * NumericTraits<PixelType>::NonpositiveMin(). */
  itkGetMacro(BackgroundValue, OutputPixelType);
  
protected:
  FastIncrementalBinaryDilateImageFilter();
  virtual ~FastIncrementalBinaryDilateImageFilter(){}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /**
   * Analyze kernel and prepare data for GenerateData() function
   */
  void AnalyzeKernel();

  /**
   * Standard pipeline method. 
   */
  void GenerateData();

  /**
   * FastIncrementalBinaryDilateImageFilter needs to request enough of
   * an input image to account for the structuring element and
   * connectivity element size.  The input requested region is
   * expanded by the maximum of the radius of the structuring element
   * and the radius used to determine connectivity (typically one).
   * If the request extends past the LargestPossibleRegion for the
   * input, the request is cropped by the LargestPossibleRegion. */
  void GenerateInputRequestedRegion() throw (InvalidRequestedRegionError);
  
private:
  FastIncrementalBinaryDilateImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** radius of neighborhood used in order to define connectivity
    * neighborhood */
  InputSizeType m_Radius;

  /** kernel or structuring element to use. */
  KernelType m_Kernel;

  /** Pixel value to dilate */
  InputPixelType m_DilateValue;

  /** Pixel value for background */
  OutputPixelType m_BackgroundValue;
  
  // type definition of container of neighbourhood index
  typedef std::vector< unsigned int > NeighborIndexContainer;

  // type definition of container of container of neighbourhood index
  typedef std::vector<NeighborIndexContainer> NeighborIndexContainerContainer;

  // Difference sets definition
  NeighborIndexContainerContainer m_KernelDifferenceSets;

  // For each Connected Component ( CC ) of structuring element we
  // store the position of one element, arbitrary chosen, which
  // belongs to the CC
  std::vector< OffsetType > m_KernelCCVectors;

  // vector of index of kernel elements which are ON
  std::vector<unsigned int> m_KernelOnElements;

  // Structure for border encoding of input binarized image
  struct BorderCell
  {
    IndexType index;
    unsigned int code;
  };

  // typedef of container of border cells
  typedef std::vector< BorderCell > BorderCellContainer;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFastIncrementalBinaryDilateImageFilter.txx"
#endif

#endif
