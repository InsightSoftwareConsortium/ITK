#ifndef __itkLabelSetDilateImageFilter_h
#define __itkLabelSetDilateImageFilter_h

#include "itkLabelSetMorphBaseImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 * \class LabelSetDilateImageFilter
 * \brief Class for binary morphological erosion of label images.
 *
 * This filter is threaded.
 *
 * \sa itkLabelSetDilateErodeImageFilter
 *
 * \author Richard Beare, Department of Medicine, Monash University,
 * Australia.  <Richard.Beare@monash.edu>
**/
template <typename TInputImage,
          typename TOutputImage= TInputImage >
class ITK_EXPORT LabelSetDilateImageFilter:
    public LabelSetMorphBaseImageFilter<TInputImage, true, TOutputImage>
{

public:
  /** Standard class typedefs. */
  typedef LabelSetDilateImageFilter  Self;
  typedef LabelSetMorphBaseImageFilter<TInputImage, true, TOutputImage> Superclass;
  typedef SmartPointer<Self>                   Pointer;
  typedef SmartPointer<const Self>        ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelSetDilateImageFilter, LabelSetMorphBaseImageFilter);

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

  typedef typename OutputImageType::RegionType OutputImageRegionType;

  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

protected:
  LabelSetDilateImageFilter(){};
  virtual ~LabelSetDilateImageFilter() {};

  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread, ThreadIdType threadId ) override;

private:
  LabelSetDilateImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  typedef typename Superclass::DistanceImageType DistanceImageType;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelSetDilateImageFilter.txx"
#endif

#endif
