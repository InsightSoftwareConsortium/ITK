/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkBSplineScatteredDataPointSetToGradientImageFilter_hxx
#define itkBSplineScatteredDataPointSetToGradientImageFilter_hxx

#include "itkBSplineScatteredDataPointSetToGradientImageFilter.h"

#include "itkImageRegionIteratorWithIndex.h"
#include "itkNeighborhoodAlgorithm.h" // face calculator
#include "itkMath.h"

namespace itk
{

template <typename TInputPointSet, typename TOutputValueType>
BSplineScatteredDataPointSetToGradientImageFilter<TInputPointSet,
                                                  TOutputValueType>::BSplineScatteredDataPointSetToGradientImageFilter()
{
  this->SetNumberOfRequiredOutputs(InputVectorDimension);
  // ImageSource only does this for the first output.
  for (unsigned int i = 1; i < InputVectorDimension; i++)
  {
    this->SetNthOutput(i, this->MakeOutput(i));
  }

  m_BSplineScatteredDataFilter = BSplineScatteredDataFilterType::New();
  m_BSplineScatteredDataFilter->SetGenerateOutputImage(false);

  this->m_SplineOrder.Fill(3);
  for (unsigned int i = 0; i < ImageDimension; i++)
  {
    this->m_NumberOfControlPoints[i] = (this->m_SplineOrder[i] + 1);
  }
  this->m_NumberOfLevels.Fill(1);
}


template <typename TInputPointSet, typename TOutputValueType>
void
BSplineScatteredDataPointSetToGradientImageFilter<TInputPointSet, TOutputValueType>::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  OutputImagePointer outputPtr;
  for (unsigned int i = 0; i < InputVectorDimension; ++i)
  {
    // get pointers to the input and output
    outputPtr = this->GetOutput(i);
    if (!outputPtr)
    {
      return;
    }

    OutputImageRegionType outputLargestPossibleRegion;
    outputLargestPossibleRegion.SetSize(this->m_Size);
    typename OutputImageRegionType::IndexType index;
    index.Fill(0);
    outputLargestPossibleRegion.SetIndex(index);
    outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);

    outputPtr->SetSpacing(this->GetSpacing());
    outputPtr->SetOrigin(this->GetOrigin());
    outputPtr->SetDirection(this->GetDirection());
  }
}


template <typename TInputPointSet, typename TOutputValueType>
void
BSplineScatteredDataPointSetToGradientImageFilter<TInputPointSet, TOutputValueType>::BeforeThreadedGenerateData()
{
  for (unsigned int i = 0; i < ImageDimension; i++)
  {
    if (this->m_Size[i] == 0)
    {
      itkExceptionMacro("Size must be specified.");
    }
  }

  m_BSplineScatteredDataFilter->SetSplineOrder(m_SplineOrder);
  m_BSplineScatteredDataFilter->SetNumberOfControlPoints(m_NumberOfControlPoints);
  m_BSplineScatteredDataFilter->SetNumberOfLevels(m_NumberOfLevels);
  m_BSplineScatteredDataFilter->SetOrigin(this->GetOrigin());
  m_BSplineScatteredDataFilter->SetSpacing(this->GetSpacing());
  m_BSplineScatteredDataFilter->SetDirection(this->GetDirection());
  m_BSplineScatteredDataFilter->SetSize(this->GetSize());
  m_BSplineScatteredDataFilter->SetInput(this->GetInput());
  m_BSplineScatteredDataFilter->Update();
}


template <typename TInputPointSet, typename TOutputValueType>
void
BSplineScatteredDataPointSetToGradientImageFilter<TInputPointSet, TOutputValueType>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegion)
{
  DataObjectPointerArray            outputs = this->GetOutputs();
  typename OutputImageType::Pointer output0 = this->GetOutput();

  typename BSplineControlPointImageFunctionType::Pointer bspliner = BSplineControlPointImageFunctionType::New();
  bspliner->SetSplineOrder(this->m_BSplineScatteredDataFilter->GetSplineOrder());
  bspliner->SetSize(this->m_BSplineScatteredDataFilter->GetSize());
  bspliner->SetSpacing(this->m_BSplineScatteredDataFilter->GetSpacing());
  bspliner->SetOrigin(this->m_BSplineScatteredDataFilter->GetOrigin());
  bspliner->SetInputImage(this->m_BSplineScatteredDataFilter->GetPhiLattice());

  using OutputIteratorType = typename itk::ImageRegionIteratorWithIndex<OutputImageType>;
  InternalGradientType                      gradient;
  OutputPixelType                           gradientPixel;
  typename OutputImageType::PointType       point;
  typename OutputImageRegionType::IndexType index;
  unsigned int                              i, j;

  // We need to know the control point spacing.
  const typename OutputImageType::SpacingType spacing = output0->GetSpacing();
  typename OutputImageType::SizeType          sizeMinus1 = this->GetSize();
  for (i = 0; i < ImageDimension; ++i)
  {
    --(sizeMinus1[i]);
  }
  typename OutputImageType::SpacingType ctrlPointSpacing;
  for (i = 0; i < ImageDimension; ++i)
  {
    ctrlPointSpacing[i] =
      static_cast<double>(sizeMinus1[i]) * spacing[i] /
      static_cast<double>(m_NumberOfControlPoints[i] * std::pow(2., static_cast<int>(m_NumberOfLevels[i] - 1)) - 1.0);
  }

  // We check to make sure we not ad the boundary of the image, because we get
  // weird edge effects and sometime we try to evaluate outside the BSpline
  // grid.
  using FaceCalculatorType = typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<OutputImageType>;
  FaceCalculatorType                      faceCalculator;
  typename FaceCalculatorType::RadiusType radius;
  radius.Fill(1);
  typename FaceCalculatorType::FaceListType           faceList = faceCalculator(output0, outputRegion, radius);
  typename FaceCalculatorType::FaceListType::iterator fit;
  fit = faceList.begin();
  typename std::vector<OutputIteratorType> outIts;
  for (i = 0; i < InputVectorDimension; i++)
  {
    OutputIteratorType outIt(dynamic_cast<OutputImageType *>(outputs[i].GetPointer()), *fit);

    outIt.GoToBegin();
    outIts.push_back(outIt);
  }
  while (!outIts[0].IsAtEnd())
  {
    index = outIts[0].GetIndex();
    output0->TransformIndexToPhysicalPoint(index, point);
    gradient = bspliner->EvaluateGradient(point);
    for (i = 0; i < InputVectorDimension; ++i)
    {
      for (j = 0; j < ImageDimension; ++j)
      {
        gradientPixel[j] = gradient(i, j) / ctrlPointSpacing[j];
      }
      outIts[i].Set(gradientPixel);
      ++(outIts[i]);
    }
  }

  unsigned int k;
  for (++fit; fit != faceList.end(); ++fit)
  {
    for (i = 0; i < InputVectorDimension; i++)
    {
      OutputIteratorType outIt(dynamic_cast<OutputImageType *>(outputs[i].GetPointer()), *fit);

      outIt.GoToBegin();
      outIts[i] = outIt;
    }
    while (!outIts[0].IsAtEnd())
    {
      index = outIts[0].GetIndex();
      for (k = 0; k < ImageDimension; ++k)
      {
        if (index[k] == 0)
        {
          index[k] = 1;
        }
        else if (static_cast<int>(index[k]) == static_cast<int>(sizeMinus1[k]))
        {
          index[k] = sizeMinus1[k] - 1;
        }
      }
      output0->TransformIndexToPhysicalPoint(index, point);
      gradient = bspliner->EvaluateGradient(point);
      for (i = 0; i < InputVectorDimension; ++i)
      {
        for (j = 0; j < ImageDimension; ++j)
        {
          gradientPixel[j] = gradient(i, j) / ctrlPointSpacing[j];
        }
        outIts[i].Set(gradientPixel);
        ++(outIts[i]);
      }
    }
  }
}

} // end namespace itk

#endif
