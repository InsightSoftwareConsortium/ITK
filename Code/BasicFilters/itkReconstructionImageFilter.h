/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReconstructionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkReconstructionImageFilter_h
#define __itkReconstructionImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"
#include <queue>

//#define BASIC
#define COPY


#ifdef COPY
#include "itkNeighborhoodAlgorithm.h"
#endif

namespace itk {

/** \class ReconstructionImageFilter 
 * \brief Performs a grayscale geodesic reconstruction -- for
 * performance comparison with GrayscaleGeodesicDilateImageFilter. 
 *
 * This filter uses Luc Vincent's algorithm, which employs raster and
 * antiraster propogation steps followed by a FIFO based propogation
 * step. "Morphological grayscale reconstruction in image analysis -
 * applications and efficient algorithms" -- IEEE Transactions on
 * Image processing, Vol 2, No 2, pp 176-201, April 1993
 *
 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 *
 * \sa MorphologicalReonstructionErosionImageFilter MorphologicalReonstructionDilationImageFilter
 * \ingroup MathematicalMorphologyImageFilters
 */

template<class TInputImage, class TOutputImage, class TCompare>
class ITK_EXPORT ReconstructionImageFilter : 
    public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef ReconstructionImageFilter Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage>
  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage InputImageType;
  typedef TInputImage MarkerImageType;
  typedef TInputImage MaskImageType;
  typedef TOutputImage OutputImageType;
  typedef typename InputImageType::SizeType        ISizeType;
  typedef typename MarkerImageType::Pointer        MarkerImagePointer;
  typedef typename MarkerImageType::ConstPointer   MarkerImageConstPointer;
  typedef typename MarkerImageType::RegionType     MarkerImageRegionType;
  typedef typename MarkerImageType::PixelType      MarkerImagePixelType;
  typedef typename InputImageType::PixelType       InputImagePixelType;
  typedef typename InputImageType::IndexType       InputImageIndexType;
  typedef typename MaskImageType::Pointer          MaskImagePointer;
  typedef typename MaskImageType::ConstPointer     MaskImageConstPointer;
  typedef typename MaskImageType::RegionType       MaskImageRegionType;
  typedef typename MaskImageType::PixelType        MaskImagePixelType;
  typedef typename OutputImageType::Pointer        OutputImagePointer;
  typedef typename OutputImageType::ConstPointer   OutputImageConstPointer;
  typedef typename OutputImageType::RegionType     OutputImageRegionType;
  typedef typename OutputImageType::PixelType      OutputImagePixelType;
  typedef typename OutputImageType::IndexType      OutputImageIndexType;
  
  /** ImageDimension constants */
  /** ImageDimension constants */
  itkStaticConstMacro(MarkerImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(MaskImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);  

  /** Runtime information support. */
  itkTypeMacro(ReconstructionImageFilter, 
               ImageToImageFilter);

  /** Set/Get the marker image. Traditionally, the marker image must
   * be pixelwise less than or equal to the mask image (for dilation),
   * however this filter implicitly applies a mask to force the
   * constraint to hold. The marker image the
   * image that is dilated by this filter. */
  void SetMarkerImage(const MarkerImageType *);
  const MarkerImageType* GetMarkerImage();

  /** Set/Get the mask image. The mask image is used to "mask" the
   * dilated marker image. The mask operation is a pixelwise
   * minimum. */
  void SetMaskImage(const MaskImageType *);
  const MaskImageType* GetMaskImage();


  /**
   * Set/Get whether the connected components are defined strictly by
   * face connectivity or by face+edge+vertex connectivity.  Default is
   * FullyConnectedOff.  For objects that are 1 pixel wide, use
   * FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);
  
  /**
   * Perform a padding of the image internally to increase the performance
   * of the filter. UseInternalCopy can be set to false to reduce the memory
   * usage.
   */
  itkSetMacro(UseInternalCopy, bool);
  itkGetConstReferenceMacro(UseInternalCopy, bool);
  itkBooleanMacro(UseInternalCopy);

protected:
  ReconstructionImageFilter();
  ~ReconstructionImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** ValuedRegionalExtremaImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ;

  /** ValuedRegionalExtremaImageFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion(DataObject *itkNotUsed(output));
  
  void GenerateData();
  
  /**
   * Set/Get the value of the border - used in boundary condition.
   */
  itkSetMacro(MarkerValue, typename TInputImage::PixelType);
  itkGetConstReferenceMacro(MarkerValue, typename TInputImage::PixelType);


private:
  ReconstructionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  typename TInputImage::PixelType m_MarkerValue;
  bool                m_FullyConnected;
  bool                m_UseInternalCopy;

  typedef typename itk::NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<OutputImageType> FaceCalculatorType;

  typedef typename FaceCalculatorType::FaceListType FaceListType;
  typedef typename FaceCalculatorType::FaceListType::iterator FaceListTypeIt;

  typedef ImageRegionConstIterator<InputImageType> InputIteratorType;
  typedef ImageRegionIterator<OutputImageType> OutputIteratorType;

  typedef typename OutputImageType::IndexType OutIndexType;
  typedef typename InputImageType::IndexType InIndexType;
  typedef ConstShapedNeighborhoodIterator<InputImageType> CNInputIterator;
  typedef ShapedNeighborhoodIterator<OutputImageType> NOutputIterator;

} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkReconstructionImageFilter.txx"
#endif


#endif
