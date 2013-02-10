#ifndef __itkLabelSetErodeImageFilter_h
#define __itkLabelSetErodeImageFilter_h

#include "itkLabelSetMorphBaseImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 * \class LabelSetErodeImageFilter
 * \brief Class for binary morphological erosion of label images.
 *
 * This filter will separate touching labels. If you don't want this
 * then use a conventional binary erosion to mask the label image.
 * This filter is threaded.
 *
 * \sa itkLabelSetDilateImageFilter
 *
 * \author Richard Beare, Department of Medicine, Monash University,
 * Australia.  <Richard.Beare@monash.edu>
**/
template <typename TInputImage,
          typename TOutputImage= TInputImage >
class ITK_EXPORT LabelSetErodeImageFilter:
    public LabelSetMorphBaseImageFilter<TInputImage, false, TOutputImage>
{

public:
  /** Standard class typedefs. */
  typedef LabelSetErodeImageFilter  Self;
  typedef LabelSetMorphBaseImageFilter<TInputImage, false, TOutputImage> Superclass;
  typedef SmartPointer<Self>                   Pointer;
  typedef SmartPointer<const Self>        ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelSetErodeImageFilter, LabelSetMorphBaseImageFilter);

  /** Pixel Type of the input image */
  typedef TInputImage                                    InputImageType;
  typedef TOutputImage                                   OutputImageType;
  typedef typename TInputImage::PixelType                PixelType;
  typedef typename NumericTraits<PixelType>::FloatType    RealType;
  typedef typename TOutputImage::PixelType  OutputPixelType;
  typedef typename NumericTraits<PixelType>::ScalarRealType ScalarRealType;

  /** Smart pointer typedef support.  */
  typedef typename TInputImage::Pointer  InputImagePointer;
  typedef typename TInputImage::ConstPointer  InputImageConstPointer;
  typedef typename TInputImage::SizeType    InputSizeType;
  typedef typename TOutputImage::SizeType   OutputSizeType;

  /** a type to represent the "kernel radius" */
  typedef typename itk::FixedArray<ScalarRealType, TInputImage::ImageDimension> RadiusType;
  /** Image dimension. */

  typedef typename OutputImageType::RegionType OutputImageRegionType;
  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);


protected:
  LabelSetErodeImageFilter(){};
  virtual ~LabelSetErodeImageFilter() {};

  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread, ThreadIdType threadId );

  // Override since the filter produces the entire dataset.

private:
  LabelSetErodeImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  typedef typename Superclass::DistanceImageType DistanceImageType;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelSetErodeImageFilter.txx"
#endif

#endif
