#ifndef __itkHistogramImageToImageMetric_txx
#define __itkHistogramImageToImageNetric_txx

#include "itkArray.h"
#include "itkHistogramImageToImageMetric.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkOptimizedBSplineDeformableTransform.h"

namespace itk
{
  template <class TFixedImage, class TMovingImage>
  HistogramImageToImageMetric<TFixedImage,TMovingImage>
  ::HistogramImageToImageMetric()
  {
    itkDebugMacro("Constructor");

    m_UsePaddingValue = false;
    m_DerivativeStepLength = 0.1;
    m_DerivativeStepLengthScales.Fill(1);
  }

  template <class TFixedImage, class TMovingImage>
  void HistogramImageToImageMetric<TFixedImage, TMovingImage>
  ::Initialize() throw (ExceptionObject)
  {
    Superclass::Initialize();

    if (!GetFixedImage())
    {
      ExceptionObject ex(__FILE__, __LINE__);
      ex.SetDescription("Fixed image has not been set.");
      throw ex;
    }
    else if (!GetMovingImage())
    {
      ExceptionObject ex(__FILE__, __LINE__);
      ex.SetDescription("Moving image has not been set.");
      throw ex;
    }

    // Calculate min and max image values in fixed image.
    FixedImageConstPointerType pFixedImage = GetFixedImage();
    FixedImagePixelType minFixed, maxFixed;
    ImageRegionConstIterator<FixedImageType> fiIt(pFixedImage,
      pFixedImage->GetBufferedRegion());
    fiIt.GoToBegin();
    minFixed = fiIt.Value();
    maxFixed = fiIt.Value();
    ++fiIt;
    while (!fiIt.IsAtEnd())
    {
      FixedImagePixelType value = fiIt.Value();
      
      if (value < minFixed)
        minFixed = value;
      else if (value > maxFixed)
        maxFixed = value;

      ++fiIt;
    }
    
    // Calculate min and max image values in moving image.
    MovingImageConstPointerType pMovingImage = GetMovingImage();
    MovingImagePixelType minMoving, maxMoving;
    ImageRegionConstIterator<MovingImageType> miIt(pMovingImage,
      pMovingImage->GetBufferedRegion());
    miIt.GoToBegin();
    minMoving = miIt.Value();
    maxMoving = miIt.Value();
    ++miIt;
    while (!miIt.IsAtEnd())
    {
      MovingImagePixelType value = miIt.Value();

      if (value < minMoving)
        minMoving = value;
      else if (value > maxMoving)
        maxMoving = value;

      ++miIt;
    }

    // Initialize the upper and lower bounds of the histogram.
    m_LowerBound[0] = minFixed;
    m_LowerBound[1] = minMoving;
    m_UpperBound[0] = maxFixed + 0.001;
    m_UpperBound[1] = maxMoving + 0.001;
  }

  template <class TFixedImage, class TMovingImage>
  typename HistogramImageToImageMetric<TFixedImage,TMovingImage>::MeasureType
  HistogramImageToImageMetric<TFixedImage,TMovingImage>
  ::GetValue(const TransformParametersType& parameters) const
  {
    itkDebugMacro("GetValue( " << parameters << " ) ");

    typename HistogramType::Pointer pHistogram = HistogramType::New();
    ComputeHistogram(parameters, *pHistogram);
    
    return EvaluateMeasure(*pHistogram);
  }

  template <class TFixedImage, class TMovingImage>
  void
  HistogramImageToImageMetric<TFixedImage,TMovingImage>
  ::GetDerivative(const TransformParametersType& parameters,
    DerivativeType& derivative) const
  {
    itkDebugMacro("GetDerivative( " << parameters << " ) ");

    typename HistogramType::Pointer pHistogram = HistogramType::New();
    ComputeHistogram(parameters, *pHistogram);

    // Calculate gradient.
    const unsigned int ParametersDimension = this->GetNumberOfParameters();
    derivative = DerivativeType(ParametersDimension);
    derivative.Fill(NumericTraits<ITK_TYPENAME
      DerivativeType::ValueType>::Zero);

    for (unsigned int i = 0; i < ParametersDimension; i++)
    {
      typename HistogramType::Pointer pHistogram2 = HistogramType::New();
      CopyHistogram(*pHistogram2, *pHistogram);
      
      TransformParametersType newParameters = parameters;
      newParameters[i] -=
        m_DerivativeStepLength/m_DerivativeStepLengthScales[i];
      ComputeHistogram(newParameters, *pHistogram2);

      MeasureType e0 = EvaluateMeasure(*pHistogram2);
      
      pHistogram2 = HistogramType::New();
      CopyHistogram(*pHistogram2, *pHistogram);

      newParameters = parameters;
      newParameters[i] +=
        m_DerivativeStepLength/m_DerivativeStepLengthScales[i];
      ComputeHistogram(newParameters, *pHistogram2);

      MeasureType e1 = EvaluateMeasure(*pHistogram2);

      derivative[i] =
        (e1 - e0)/(2*m_DerivativeStepLength/m_DerivativeStepLengthScales[i]);
    }
  }

  template <class TFixedImage, class TMovingImage>
  void
  HistogramImageToImageMetric<TFixedImage,TMovingImage>
  ::GetValueAndDerivative(const TransformParametersType& parameters,
    MeasureType& value,
    DerivativeType& derivative) const
  {
    value = GetValue(parameters);
    GetDerivative(parameters, derivative);
  }

  template <class TFixedImage, class TMovingImage>
  void
  HistogramImageToImageMetric<TFixedImage,TMovingImage>
  ::ComputeHistogram(TransformParametersType const& parameters,
    HistogramType& histogram) const
  {
    FixedImageConstPointerType fixedImage = this->GetFixedImage();
    
    if(!fixedImage)
      itkExceptionMacro(<< "Fixed image has not been assigned");
    
    typedef itk::ImageRegionConstIteratorWithIndex<FixedImageType>
      FixedIteratorType;
    typename FixedImageType::IndexType index;
    typename FixedImageType::RegionType fixedRegion;

    fixedRegion = this->GetFixedImageRegion();
    FixedIteratorType ti(fixedImage, fixedRegion);
    
    m_NumberOfPixelsCounted = 0;
    this->SetTransformParameters(parameters);
    
    histogram.Initialize(m_HistogramSize, m_LowerBound, m_UpperBound);
    
    ti.GoToBegin();
    while (!ti.IsAtEnd())
    {
      index = ti.GetIndex();
      
      if (fixedRegion.IsInside(index) &&
        (!m_UsePaddingValue ||
          (m_UsePaddingValue && ti.Get() > m_PaddingValue)))
      {
        typename Superclass::InputPointType inputPoint;
        fixedImage->TransformIndexToPhysicalPoint(index, inputPoint);
        
        typename Superclass::OutputPointType transformedPoint =
          m_Transform->TransformPoint(inputPoint);

        if (m_Interpolator->IsInsideBuffer(transformedPoint))
        {
          const RealType movingValue =
            m_Interpolator->Evaluate(transformedPoint);
          const RealType fixedValue = ti.Get();
          m_NumberOfPixelsCounted++;
          
          typename HistogramType::MeasurementVectorType sample;
          sample[0] = fixedValue;
          sample[1] = movingValue;
          histogram.IncreaseFrequency(sample, 1);
        }
      }
      
      ++ti;
    }
    
    if (m_NumberOfPixelsCounted == 0)
      itkExceptionMacro(<< "All the points mapped to outside of the moving \
image");
  }

  template <class TFixedImage, class TMovingImage>
  void
  HistogramImageToImageMetric<TFixedImage,TMovingImage>
  ::CopyHistogram(HistogramType& target, HistogramType& source) const
  {
    // Initialize the target.
    typename HistogramType::MeasurementVectorType min, max;
    typename HistogramType::SizeType size = source.GetSize();

    for (unsigned int i = 0; i < min.Size(); i++)
      min[i] = source.GetBinMin(i, 0);
    for (unsigned int i = 0; i < max.Size(); i++)
      max[i] = source.GetBinMax(i, size[i] - 1);

    target.Initialize(size, min, max);

    // Copy the values.
    typename HistogramType::Iterator sourceIt = source.Begin();
    typename HistogramType::Iterator sourceEnd = source.End();
    typename HistogramType::Iterator targetIt = target.Begin();
    typename HistogramType::Iterator targetEnd = target.End();

    while (sourceIt != sourceEnd && targetIt != targetEnd)
    {
      typename HistogramType::FrequencyType freq = sourceIt.GetFrequency();
      
      if (freq > 0)
        targetIt.SetFrequency(freq);
      
      ++sourceIt;
      ++targetIt;
    }
  }
} // end namespace itk

#endif // itkHistogramImageToImageMetric_txx
