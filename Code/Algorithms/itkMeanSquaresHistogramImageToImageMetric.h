#ifndef __itkMeanSquaresHistogramImageToImageMetric_h
#define __itkMeanSquaresHistogramImageToImageMetric_h

#include "itkHistogramImageToImageMetric.h"

namespace itk
{
  /** \class MeanSquaresHistogramImageToImageMetric
      \brief Computes mean squared difference similarity measure between two
      images to be registered.

      This class is templated over the type of the fixed and moving
      images to be compared.

      This metric computes the similarity measure between pixels in the
      moving image and pixels in the fixed images using a histogram.

      \ingroup RegistrationMetrics */
template <class TFixedImage, class TMovingImage>
class ITK_EXPORT MeanSquaresHistogramImageToImageMetric :
public HistogramImageToImageMetric<TFixedImage, TMovingImage>
{
 public:
  /** Standard class typedefs. */
  typedef MeanSquaresHistogramImageToImageMetric Self;
  typedef HistogramImageToImageMetric<TFixedImage, TMovingImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MeanSquaresHistogramImageToImageMetric,
    HistogramImageToImageMetric);

  /** Types transferred from the base class */
  typedef typename Superclass::RealType                 RealType;
  typedef typename Superclass::TransformType            TransformType;
  typedef typename Superclass::TransformPointer         TransformPointer;
  typedef typename Superclass::TransformParametersType  TransformParametersType;
  typedef typename Superclass::TransformJacobianType    TransformJacobianType;
  typedef typename Superclass::GradientPixelType        GradientPixelType;

  typedef typename Superclass::MeasureType              MeasureType;
  typedef typename Superclass::DerivativeType           DerivativeType;
  typedef typename Superclass::FixedImageType           FixedImageType;
  typedef typename Superclass::MovingImageType          MovingImageType;
  typedef typename Superclass::FixedImageConstPointer   FixedImageConstPointer;
  typedef typename Superclass::MovingImageConstPointer  MovingImageConstPointer;

  typedef typename Superclass::HistogramType            HistogramType;
  typedef typename HistogramType::FrequencyType         HistogramFrequencyType;
  typedef typename HistogramType::Iterator              HistogramIteratorType;
  typedef typename HistogramType::MeasurementVectorType HistogramMeasurementVectorType;

protected:
  /** Constructor is protected to ensure that \c New() function is used to
      create instances. */
  MeanSquaresHistogramImageToImageMetric(){}
  virtual ~MeanSquaresHistogramImageToImageMetric(){}
  
  /** Evaluates the sum of squared differences from the histogram. */
  virtual MeasureType EvaluateMeasure(HistogramType& histogram) const;

private:
  MeanSquaresHistogramImageToImageMetric(Self const&); // Purposely not implemented.
  void operator=(Self const&); // Purposely not implemented.
};

} // End namespace itk.

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeanSquaresHistogramImageToImageMetric.txx"
#endif

#endif // __itkMeanSquaresHistogramImageToImageMetric_h
