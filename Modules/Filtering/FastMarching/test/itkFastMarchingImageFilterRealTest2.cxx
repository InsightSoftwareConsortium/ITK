/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/


#include "itkFastMarchingImageToNodePairContainerAdaptor.h"
#include "itkFastMarchingImageFilterBase.h"
#include "itkFastMarchingThresholdStoppingCriterion.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkTextOutput.h"
#include "itkTestingMacros.h"
#include "itkCommand.h"


namespace
{
// The following class is used to support callbacks
// on the filter in the pipeline that follows later
class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject * o) { m_Process = o; }
  void
  ShowProgress()
  {
    std::cout << "Progress " << m_Process->GetProgress() << std::endl;
  }
  itk::ProcessObject::Pointer m_Process;
};
} // namespace

int
itkFastMarchingImageFilterRealTest2(int itkNotUsed(argc), char * itkNotUsed(argv)[])
{
  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());

  // Create a Fast Marching image filter object
  using PixelType = float;
  constexpr unsigned int Dimension = 2;

  using FloatImageType = itk::Image<PixelType, Dimension>;

  using CriterionType = itk::FastMarchingThresholdStoppingCriterion<FloatImageType, FloatImageType>;

  using FastMarchingType = itk::FastMarchingImageFilterBase<FloatImageType, FloatImageType>;

  auto criterion = CriterionType::New();
  criterion->SetThreshold(100.);

  auto marcher = FastMarchingType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(marcher, FastMarchingImageFilterBase, FastMarchingBase);


  marcher->SetStoppingCriterion(criterion);

  ShowProgressObject                                    progressWatch(marcher);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch, &ShowProgressObject::ShowProgress);
  marcher->AddObserver(itk::ProgressEvent(), command);

  // Specify the size of the output image
  FloatImageType::SizeType size = { { 64, 64 } };
  marcher->SetOutputSize(size);

  // Set up a speed image of ones
  auto                       speedImage = FloatImageType::New();
  FloatImageType::RegionType region;
  region.SetSize(size);
  speedImage->SetLargestPossibleRegion(region);
  speedImage->SetBufferedRegion(region);
  speedImage->Allocate();

  // Set up an 'alive image'
  auto aliveImage = FloatImageType::New();
  aliveImage->SetLargestPossibleRegion(region);
  aliveImage->SetBufferedRegion(region);
  aliveImage->Allocate();
  aliveImage->FillBuffer(0.0);

  FloatImageType::OffsetType offset0 = { { 28, 35 } };

  itk::Index<Dimension> index;
  index.Fill(0);
  index += offset0;

  aliveImage->SetPixel(index, 1.0);

  // Set up a 'trial image'
  auto trialImage = FloatImageType::New();
  trialImage->SetLargestPossibleRegion(region);
  trialImage->SetBufferedRegion(region);
  trialImage->Allocate();
  trialImage->FillBuffer(0.0);

  index[0] += 1;
  trialImage->SetPixel(index, 1.0);

  index[0] -= 1;
  index[1] += 1;
  trialImage->SetPixel(index, 1.0);

  index[0] -= 1;
  index[1] -= 1;
  trialImage->SetPixel(index, 1.0);

  index[0] += 1;
  index[1] -= 1;
  trialImage->SetPixel(index, 1.0);

  // Set up a binary mask image in float (to make sure it works with float)
  auto maskImage = FloatImageType::New();
  maskImage->SetLargestPossibleRegion(region);
  maskImage->SetBufferedRegion(region);
  maskImage->Allocate();

  itk::ImageRegionIterator<FloatImageType>          speedIter(speedImage, speedImage->GetBufferedRegion());
  itk::ImageRegionIteratorWithIndex<FloatImageType> maskIter(maskImage, maskImage->GetBufferedRegion());
  while (!speedIter.IsAtEnd())
  {
    speedIter.Set(1.0);
    FloatImageType::IndexType idx = maskIter.GetIndex();
    if (((idx[0] > 22) && (idx[0] < 42) && (idx[1] > 27) && (idx[1] < 37)) ||
        ((idx[1] > 22) && (idx[1] < 42) && (idx[0] > 27) && (idx[0] < 37)))
    {
      maskIter.Set(1.0);
    }
    else
    {
      maskIter.Set(0.0);
    }

    ++maskIter;
    ++speedIter;
  }

  marcher->SetInput(speedImage);

  using AdaptorType = itk::FastMarchingImageToNodePairContainerAdaptor<FloatImageType, FloatImageType, FloatImageType>;

  auto adaptor = AdaptorType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(adaptor, FastMarchingImageToNodePairContainerAdaptor, Object);


  bool isForbiddenImageBinaryMask = true;
  ITK_TEST_SET_GET_BOOLEAN(adaptor, IsForbiddenImageBinaryMask, isForbiddenImageBinaryMask);

  adaptor->SetAliveImage(aliveImage.GetPointer());
  ITK_TEST_SET_GET_VALUE(aliveImage.GetPointer(), adaptor->GetAliveImage());

  typename AdaptorType::OutputPixelType aliveValue = 0.0;
  adaptor->SetAliveValue(aliveValue);
  ITK_TEST_SET_GET_VALUE(aliveValue, adaptor->GetAliveValue());

  adaptor->SetTrialImage(trialImage.GetPointer());
  ITK_TEST_SET_GET_VALUE(trialImage.GetPointer(), adaptor->GetTrialImage());

  typename AdaptorType::OutputPixelType trialValue = 1.0;
  adaptor->SetTrialValue(trialValue);
  ITK_TEST_SET_GET_VALUE(trialValue, adaptor->GetTrialValue());

  adaptor->SetForbiddenImage(maskImage.GetPointer());
  ITK_TEST_SET_GET_VALUE(maskImage.GetPointer(), adaptor->GetForbiddenImage());

  ITK_TRY_EXPECT_NO_EXCEPTION(adaptor->Update());


  marcher->SetForbiddenPoints(adaptor->GetForbiddenPoints());
  ITK_TEST_SET_GET_VALUE(adaptor->GetForbiddenPoints(), marcher->GetForbiddenPoints());

  marcher->SetAlivePoints(adaptor->GetAlivePoints());
  marcher->SetTrialPoints(adaptor->GetTrialPoints());

  // Turn on debugging
  marcher->DebugOn();

  // Update the Fast Marching filter
  ITK_TRY_EXPECT_NO_EXCEPTION(marcher->Update());


  // Check the results
  FloatImageType::Pointer output = marcher->GetOutput();

  itk::ImageRegionIterator<FloatImageType> iterator(output, output->GetBufferedRegion());

  bool passed = true;

  double threshold = 1.42;
  while (!iterator.IsAtEnd())
  {
    FloatImageType::IndexType tempIndex = iterator.GetIndex();
    auto                      outputValue = static_cast<double>(iterator.Get());

    if (((tempIndex[0] > 22) && (tempIndex[0] < 42) && (tempIndex[1] > 27) && (tempIndex[1] < 37)) ||
        ((tempIndex[1] > 22) && (tempIndex[1] < 42) && (tempIndex[0] > 27) && (tempIndex[0] < 37)))
    {
      tempIndex -= offset0;
      double distance = 0.0;
      for (unsigned int j = 0; j < Dimension; ++j)
      {
        distance += tempIndex[j] * tempIndex[j];
      }
      distance = std::sqrt(distance);

      if (distance > itk::NumericTraits<double>::epsilon())
      {
        if (itk::Math::abs(outputValue) / distance > threshold)
        {
          std::cout << "Error at index [" << iterator.GetIndex() << "]" << std::endl;
          std::cout << "Expected scaled output value be less than: " << threshold
                    << ", but got: " << itk::Math::abs(outputValue) / distance
                    << ", where output: " << itk::Math::abs(outputValue) << "; scale factor: " << distance << std::endl;
          passed = false;
        }
      }
    }
    else
    {
      if (outputValue != 0.)
      {
        std::cout << "Error at index [" << iterator.GetIndex() << "]" << std::endl;
        std::cout << "Expected output value: " << 0. << ", but got: " << outputValue << std::endl;
        passed = false;
      }
    }
    ++iterator;
  }


  if (passed)
  {
    std::cout << "Test passed!" << std::endl;
    return EXIT_SUCCESS;
  }
  else
  {
    std::cout << "Test failed!" << std::endl;
    return EXIT_FAILURE;
  }
}
