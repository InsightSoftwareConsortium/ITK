/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkDirectedHausdorffDistanceImageFilter_hxx
#define itkDirectedHausdorffDistanceImageFilter_hxx

#include "itkDirectedHausdorffDistanceImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkSignedMaurerDistanceMapImageFilter.h"
#include "itkProgressReporter.h"
#include "itkMacro.h"
#include "itkMath.h"
#include "itkPrintHelper.h"

namespace itk
{
template <typename TInputImage1, typename TInputImage2>
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>::DirectedHausdorffDistanceImageFilter()
{
  // this filter requires two input images
  this->SetNumberOfRequiredInputs(2);

  this->DynamicMultiThreadingOn();
  this->ThreaderUpdateProgressOff();
}

template <typename TInputImage1, typename TInputImage2>
void
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>::SetInput1(const TInputImage1 * image)
{
  this->SetInput(image);
}

template <typename TInputImage1, typename TInputImage2>
void
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>::SetInput2(const TInputImage2 * image)
{
  this->SetNthInput(1, const_cast<TInputImage2 *>(image));
}

template <typename TInputImage1, typename TInputImage2>
const typename DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>::InputImage1Type *
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>::GetInput1()
{
  return this->GetInput();
}

template <typename TInputImage1, typename TInputImage2>
const typename DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>::InputImage2Type *
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>::GetInput2()
{
  return itkDynamicCastInDebugMode<const TInputImage2 *>(this->ProcessObject::GetInput(1));
}

template <typename TInputImage1, typename TInputImage2>
void
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  // this filter requires:
  // - the largest possible region of the first image
  // - the corresponding region of the second image
  if (this->GetInput1())
  {
    InputImage1Pointer image1 = const_cast<InputImage1Type *>(this->GetInput1());
    image1->SetRequestedRegionToLargestPossibleRegion();

    if (this->GetInput2())
    {
      InputImage2Pointer image2 = const_cast<InputImage2Type *>(this->GetInput2());
      RegionType         region = image1->GetRequestedRegion();
      image2->SetRequestedRegion(region);
    }
  }
}

template <typename TInputImage1, typename TInputImage2>
void
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>::EnlargeOutputRequestedRegion(DataObject * data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}

template <typename TInputImage1, typename TInputImage2>
void
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>::AllocateOutputs()
{
  // Pass the first input through as the output
  InputImage1Pointer image = const_cast<TInputImage1 *>(this->GetInput1());

  this->GraftOutput(image);
}

template <typename TInputImage1, typename TInputImage2>
void
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>::BeforeThreadedGenerateData()
{
  // initialize accumulators
  m_MaxDistance = NumericTraits<RealType>::ZeroValue();
  m_PixelCount = 0;
  m_Sum = 0;

  // Compute distance from non-zero pixels in the second image
  using FilterType = itk::SignedMaurerDistanceMapImageFilter<InputImage2Type, DistanceMapType>;
  typename FilterType::Pointer filter = FilterType::New();

  auto input2 = InputImage2Type::New();
  input2->Graft(const_cast<InputImage2Type *>(this->GetInput2()));

  filter->SetInput(input2);
  filter->SetSquaredDistance(false);
  filter->SetUseImageSpacing(m_UseImageSpacing);
  filter->Update();

  m_DistanceMap = filter->GetOutput();
}

template <typename TInputImage1, typename TInputImage2>
void
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>::AfterThreadedGenerateData()
{

  if (m_PixelCount != 0)
  {
    m_AverageHausdorffDistance = m_Sum.GetSum() / static_cast<RealType>(m_PixelCount);
  }
  else
  {
    itkGenericExceptionMacro(<< "pixelcount is equal to 0");
  }

  m_DirectedHausdorffDistance = m_MaxDistance;
  // clean up
  m_DistanceMap = nullptr;
}

template <typename TInputImage1, typename TInputImage2>
void
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>::DynamicThreadedGenerateData(
  const RegionType & regionForThread)
{
  const auto *                              inputPtr1 = this->GetInput1();
  ImageRegionConstIterator<TInputImage1>    it1(inputPtr1, regionForThread);
  ImageRegionConstIterator<DistanceMapType> it2(m_DistanceMap, regionForThread);

  RealType                 maxDistance = NumericTraits<RealType>::ZeroValue();
  CompensatedSummationType sum = 0.0;
  IdentifierType           pixelCount = 0;

  // support progress methods/callbacks
  TotalProgressReporter progress(this, inputPtr1->GetRequestedRegion().GetNumberOfPixels());

  // do the work
  while (!it1.IsAtEnd())
  {
    if (Math::NotExactlyEquals(it1.Get(), NumericTraits<InputImage1PixelType>::ZeroValue()))
    {
      // The signed distance map is calculated, but we want the calculation based on the
      // unsigned distance map.  Therefore, we set all distance map values less than 0 to 0.
      const RealType val2 = std::max(static_cast<RealType>(it2.Get()), NumericTraits<RealType>::ZeroValue());
      maxDistance = std::max(maxDistance, val2);
      sum += val2;
      ++pixelCount;
    }

    ++it1;
    ++it2;

    progress.CompletedPixel();
  }
  std::lock_guard<std::mutex> mutexHolder(m_Mutex);
  m_MaxDistance = std::max(m_MaxDistance, maxDistance);
  m_Sum += sum;
  m_PixelCount += pixelCount;
}

template <typename TInputImage1, typename TInputImage2>
void
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>::PrintSelf(std::ostream & os, Indent indent) const
{
  using namespace print_helper;

  Superclass::PrintSelf(os, indent);

  os << indent << "DistanceMap: " << m_DistanceMap << std::endl;
  os << indent << "MaxDistance: " << m_MaxDistance << std::endl;
  os << indent << "PixelCount:" << m_PixelCount << std::endl;
  os << indent << "Sum: " << m_Sum.GetSum();

  os << std::endl;
  os << indent << "DirectedHausdorffDistance: "
     << static_cast<typename NumericTraits<RealType>::PrintType>(m_DirectedHausdorffDistance) << std::endl;
  os << indent << "AverageHausdorffDistance: "
     << static_cast<typename NumericTraits<RealType>::PrintType>(m_AverageHausdorffDistance) << std::endl;
  os << indent << "UseImageSpacing: " << m_UseImageSpacing << std::endl;
}
} // end namespace itk
#endif
