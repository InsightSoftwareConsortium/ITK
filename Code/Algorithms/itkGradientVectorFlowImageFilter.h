#ifndef __itkGradientVectorFlowImageFilter_h
#define __itkGradientVectorFlowImageFilter_h

#include "itkImageToImageFilter.h"
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/vnl_math.h"
#include "itkImage.h"
#include "itkVector.h"
#include "itkLaplacianImageFilter.h"
#include "itkSimpleImageRegionIterator.h"
#include <itkCovariantVector.h>

namespace itk
{

/** \class GradientVectorFlow
 * \brief 
 *
 *
 *
 * \ingroup ImageFilters
 * \ingroup ImageSegmentation */
template <class TInputImage, class TOutputImage>
class GradientVectorFlowImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard "Self" typedef. */
  typedef GradientVectorFlowImageFilter  Self;

  /** Standard "Superclass" typedef. */
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;

  /** Smart pointer typedef support */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method of creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(GradientVectorFlowImageFilter, ImageToImageFilter);

  /** Some typedefs. */
  typedef TInputImage InputImageType;
  typedef TOutputImage OutputImageType;
  typedef LapacianImageFilter<TInputImage, TInputImage> LapacianFilterType;
  typedef typename TInputImage::IndexType IndexType;
  typedef typename TInputImage::SizeType SizeType;
  typedef typename TInputImage::PixelType PixelType;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef typename OutputImageType::RegionType RegionType;
  typedef typename LapacianFilterType::Pointer LapacianFilterPointer;

  /** Image and Image iterator definition. */
  typedef SimpleImageRegionIterator<InputImageType> InputImageIterator;
  typedef SimpleImageRegionIterator<OutputImageType> OutputImageIterator;

  /** Image dimension. */
  enum { ImageDimension = TInputImage::ImageDimension };

  typedef ConvariantVector<double, ImageDimension> GradientType;

  /** Routines. */
  

  /** Set/Get routines. */
  itkSetMacro(OutputImage, OutputImagePointer);
  itkGetMacro(OutputImage, OutputImagePointer);

  itkSetMacro(LapacianFilter, LapacianFilterPointer);

  itkSetMacro(TimeStep, double);

  itkSetMacro(Steps, double[ImageDimension]);

  itkSetMacro(NoiseLevel, double);

protected:
  GradientVectorFlowImageFilter();
  ~GradientVectorFlowImageFilter() {}
  GradientVectorFlowImageFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  virtual void GenerateData();

private:
  // parameters;
  double m_TimeStep;
  double m_Steps[ImageDimension];
  double m_NoiseLevel;

  LapacianFilterPointer m_LapacianFilter;
  InputImagePointer m_IntermediateImage;
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientVectorFlowImageFilter.txx"
#endif

#endif
