#ifndef __itkParabolicDilateImageFilter_h
#define __itkParabolicDilateImageFilter_h

#include "itkParabolicErodeDilateImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 * \class ParabolicDilateImageFilter
 * \brief Class for morphological dilation
 * operations  with parabolic structuring elements.
 *
 * This filter is threaded.
 *
 * \sa itkParabolicOpenCloseImageFilter
 *
 * \author Richard Beare, Department of Medicine, Monash University,
 * Australia.  <Richard.Beare@monash.edu>
 *
 **/

template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_EXPORT ParabolicDilateImageFilter : public ParabolicErodeDilateImageFilter<TInputImage, true, TOutputImage>
{

public:
  /** Standard class typedefs. */
  typedef ParabolicDilateImageFilter                                       Self;
  typedef ParabolicErodeDilateImageFilter<TInputImage, true, TOutputImage> Superclass;
  typedef SmartPointer<Self>                                               Pointer;
  typedef SmartPointer<const Self>                                         ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ParabolicDilateImageFilter, ParabolicErodeDilateImageFilter);


  /** Pixel Type of the input image */
  typedef TInputImage                                       InputImageType;
  typedef TOutputImage                                      OutputImageType;
  typedef typename TInputImage::PixelType                   PixelType;
  typedef typename NumericTraits<PixelType>::RealType       RealType;
  typedef typename NumericTraits<PixelType>::ScalarRealType ScalarRealType;
  typedef typename TOutputImage::PixelType                  OutputPixelType;

  /** Smart pointer typedef support.  */
  typedef typename TInputImage::Pointer      InputImagePointer;
  typedef typename TInputImage::ConstPointer InputImageConstPointer;

  /** a type to represent the "kernel radius" */
  typedef typename itk::FixedArray<ScalarRealType, TInputImage::ImageDimension> RadiusType;

  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

protected:
  ParabolicDilateImageFilter() {};
  virtual ~ParabolicDilateImageFilter() {};
  //   void PrintSelf(std::ostream& os, Indent indent) const;

private:
  ParabolicDilateImageFilter(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented
};

} // end namespace itk

#endif
