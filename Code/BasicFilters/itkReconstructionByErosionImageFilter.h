/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReconstructionByErosionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkReconstructionByErosionImageFilter_h
#define __itkReconstructionByErosionImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk {

/** \class ReconstructionByErosionImageFilter
 * \brief grayscale reconstruction by erosion of an image
 *
 * Reconstruction by erosion operates on a "marker" image and a "mask"
 * image, and is defined as the erosion of the marker image with
 * respect to the mask image iterated until stability.
 *
 * The marker image must be greater than or equal to the mask image
 * (on a pixel by pixel basis).
 *
 * Geodesic morphology is described in Chapter 6.2 of Pierre Soille's
 * book "Morphological Image Analysis: Principles and Applications",
 * Second Edition, Springer, 2003.
 * 
 * Algorithm implemented in this filter is based on algorithm described
 * by Kevin Robinson and  Paul F. Whelan in "Efficient Morphological 
 * Reconstruction: A Downhill Filter", Pattern Recognition Letters, Volume
 * 25, Issue 15, November 2004, Pages 1759-1767.
 *
 * Good description and applications can be found in "Morphological
 * Grayscale Reconstruction in Image Analysis: Applications and Efficient
 * Algorithms", Luc Vincent, IEEE Transactions on image processing, Vol. 2,
 * April 1993.
 *
 * \author Gaëtan Lehmann. Biologie du Développement et de la Reproduction, INRA of Jouy-en-Josas, France.
 *
 * \sa MorphologyImageFilter, GrayscaleDilateImageFilter, GrayscaleFunctionDilateImageFilter, BinaryDilateImageFilter, ReconstructionByDilationImageFilter, OpeningByReconstructionImageFilter, ClosingByReconstructionImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template<class TInputImage, class TOutputImage>
class ITK_EXPORT ReconstructionByErosionImageFilter : 
    public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef ReconstructionByErosionImageFilter Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage>
  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage MarkerImageType;
  typedef TInputImage MaskImageType;
  typedef TOutputImage OutputImageType;
  typedef typename MarkerImageType::Pointer        MarkerImagePointer;
  typedef typename MarkerImageType::ConstPointer   MarkerImageConstPointer;
  typedef typename MarkerImageType::RegionType     MarkerImageRegionType;
  typedef typename MarkerImageType::PixelType      MarkerImagePixelType;
  typedef typename MaskImageType::Pointer          MaskImagePointer;
  typedef typename MaskImageType::ConstPointer     MaskImageConstPointer;
  typedef typename MaskImageType::RegionType       MaskImageRegionType;
  typedef typename MaskImageType::PixelType        MaskImagePixelType;
  typedef typename OutputImageType::Pointer        OutputImagePointer;
  typedef typename OutputImageType::ConstPointer   OutputImageConstPointer;
  typedef typename OutputImageType::RegionType     OutputImageRegionType;
  typedef typename OutputImageType::PixelType      OutputImagePixelType;
  
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
  itkTypeMacro(ReconstructionByErosionImageFilter, 
               ImageToImageFilter);
  
  /** Set/Get the marker image. The marker image must be pixelwise
   * greater than or equal to the mask image. The marker image the
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

protected:
  ReconstructionByErosionImageFilter();
  ~ReconstructionByErosionImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** ReconstructionByErosionImageFilter needs to request enough of the
   * marker image to account for the elementary structuring element.
   * The mask image does not need to be padded. Depending on whether
   * the filter is configured to run a single iteration or until
   * convergence, this method may request all of the marker and mask
   * image be provided. */
  void GenerateInputRequestedRegion();

  /** This filter will enlarge the output requested region to produce
   * all of the output if the filter is configured to run to
   * convergence.
   * \sa ProcessObject::EnlargeOutputRequestedRegion() */
  void EnlargeOutputRequestedRegion(DataObject *itkNotUsed(output));

  /** Single-threaded version of GenerateData.  This version is used
   * when the filter is configured to run to convergence. This method
   * may delegate to the multithreaded version if the filter is
   * configured to run a single iteration.  Otherwise, it will
   * delegate to a separate instance to run each iteration until the
   * filter converges. */
  void GenerateData();
  
private:
  ReconstructionByErosionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  bool m_FullyConnected;

} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkReconstructionByErosionImageFilter.txx"
#endif

#endif


