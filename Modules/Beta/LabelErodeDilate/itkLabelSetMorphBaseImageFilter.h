#ifndef __itkLabelSetMorphBaseImageFilter_h
#define __itkLabelSetMorphBaseImageFilter_h

#include "itkNumericTraits.h"

namespace itk
{
/**
 * \class LabelSetMorphBaseImageFilter
 * \brief Base class for binary morphological erosion of label images.
 *
 * This filter is threaded. This class handles the threading for subclasses.
 *
 * \sa itkLabelSetDilateImageFilter itkLabelSetErodeImageFilter
 *
 * \author Richard Beare, Department of Medicine, Monash University,
 * Australia.  <Richard.Beare@monash.edu>
**/
template <typename TInputImage, bool doDilate,
          typename TOutputImage= TInputImage >
class ITK_EXPORT LabelSetMorphBaseImageFilter:
    public ImageToImageFilter<TInputImage, TOutputImage>
{

public:
  /** Standard class typedefs. */
  typedef LabelSetMorphBaseImageFilter  Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                   Pointer;
  typedef SmartPointer<const Self>        ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelSetMorphBaseImageFilter, ImageToImageFilter);


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

  // set all of the scales the same
  void SetRadius(ScalarRealType scale);
  itkSetMacro(Radius, RadiusType);
  itkGetConstReferenceMacro(Radius, RadiusType);

  /**
   * Set/Get whether the scale refers to pixels or world units -
   * default is false
   */
  itkSetMacro(UseImageSpacing, bool);
  itkGetConstReferenceMacro(UseImageSpacing, bool);
  itkBooleanMacro(UseImageSpacing);


  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Define the image type for internal computations
      RealType is usually 'double' in NumericTraits.
      Here we prefer float in order to save memory.  */

  void writeDist(std::string fname);

protected:
  LabelSetMorphBaseImageFilter();
  virtual ~LabelSetMorphBaseImageFilter() {};

  int SplitRequestedRegion(int i, int num, OutputImageRegionType& splitRegion);
  virtual void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread, ThreadIdType threadId){};
  void GenerateData( void );

  // Override since the filter produces the entire dataset.
  void EnlargeOutputRequestedRegion(DataObject *output);
  bool m_UseImageSpacing;
  void PrintSelf(std::ostream& os, Indent indent) const;

  RadiusType m_Radius;
  RadiusType m_Scale;
  typedef typename itk::Image<RealType, TInputImage::ImageDimension> DistanceImageType;
  typename TInputImage::PixelType m_Extreme;

  typename DistanceImageType::Pointer m_DistanceImage;
  int m_MagnitudeSign;
  int m_CurrentDimension;
  bool m_FirstPassDone;

private:
  LabelSetMorphBaseImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelSetMorphBaseImageFilter.txx"
#endif

#endif
