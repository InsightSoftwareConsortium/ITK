#ifndef __itkReconstructionByErosionImageFilter_h
#define __itkReconstructionByErosionImageFilter_h

#include "itkReconstructionImageFilter.h"

#include "itkNumericTraits.h"

namespace itk {
/** \class ReconstructionByErosionImageFilter
 * \brief grayscale reconstruction by erosion of an image
 *
 * Reconstruction by erosion operates on a "marker" image and a "mask"
 * image, and is defined as the erosion of the marker image with
 * respect to the mask image iterated until stability.
 *
 * The marker image must be less than or equal to the mask image
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
 * The algorithm, a description of the transform and some applications
 * can be found in "Morphological Grayscale Reconstruction in Image Analysis:
 * Applications and Efficient Algorithms", Luc Vincent, IEEE Transactions on
 * image processing, Vol. 2, April 1993.
 *
 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 *
 * \sa MorphologyImageFilter, GrayscaleDilateImageFilter, GrayscaleFunctionDilateImageFilter, BinaryDilateImageFilter, ReconstructionByErosionImageFilter,
OpeningByReconstructionImageFilter, ClosingByReconstructionImageFilter, ReconstructionImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup MathematicalMorphologyImageFilters
 */

template <class TInputImage, class TOutputImage>
class ITK_EXPORT ReconstructionByErosionImageFilter :
  public ReconstructionImageFilter<TInputImage, TOutputImage, std::less<typename TOutputImage::PixelType> >
{
public:
  typedef ReconstructionByErosionImageFilter Self;
  typedef ReconstructionImageFilter<TInputImage, TOutputImage, std::less<typename TOutputImage::PixelType> > Superclass;

  typedef SmartPointer<Self>   Pointer;
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

  /** Method for creation through the object factory. */
  itkNewMacro(Self);


protected:
  ReconstructionByErosionImageFilter()
    {
    this->SetMarkerValue( NumericTraits<typename TOutputImage::PixelType>::max() );
    }
  virtual ~ReconstructionByErosionImageFilter() {}

private:
  ReconstructionByErosionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented



}; // end ReconstructionByErosionImageFilter



}

#endif
