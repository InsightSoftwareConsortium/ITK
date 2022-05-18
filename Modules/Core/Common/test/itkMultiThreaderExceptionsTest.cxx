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

#include "itkImageSource.h"
#include "itkTestingMacros.h"

using ThreaderEnum = itk::MultiThreaderBase::ThreaderEnum;

namespace itk
{
template <typename TOutputImage>
class ITK_TEMPLATE_EXPORT DummyImageSource : public ImageSource<TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DummyImageSource);

  /** Standard class type aliases. */
  using Self = DummyImageSource;
  using Superclass = ImageSource<TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Convenient type alias. */
  using OutputImageRegionType = typename TOutputImage::RegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DummyImageSource, ImageSource);

  /** Index for which exception is thrown. */
  itkSetMacro(ExceptionIndex, IndexValueType);

protected:
  DummyImageSource() = default;
  ~DummyImageSource() override = default;
  void
  GenerateOutputInformation() override
  {
    TOutputImage * output = nullptr;
    output = this->GetOutput(0);
    typename TOutputImage::RegionType largestPossibleRegion;
    largestPossibleRegion.SetSize({ { 4 } });
    output->SetLargestPossibleRegion(largestPossibleRegion);
  }

  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override
  {
    if (outputRegionForThread.GetIndex(0) == m_ExceptionIndex)
    {
      std::cout << "Exception launched" << std::endl;
      itkGenericExceptionMacro(<< "Error");
    }
  }

private:
  IndexValueType m_ExceptionIndex{ 0 };
};
} // end namespace itk

int
itkMultiThreaderExceptionsTest(int, char *[])
{
  using OutputPixelType = float;
  constexpr unsigned int Dimension = 1;

  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  std::set<ThreaderEnum> threadersToTest = { ThreaderEnum::Platform, ThreaderEnum::Pool };
#ifdef ITK_USE_TBB
  threadersToTest.insert(ThreaderEnum::TBB);
#endif // ITK_USE_TBB
  for (auto thType : threadersToTest)
  {
    itk::MultiThreaderBase::SetGlobalDefaultThreader(thType);
    typename itk::DummyImageSource<OutputImageType>::Pointer dummySrc;
    dummySrc = itk::DummyImageSource<OutputImageType>::New();
    dummySrc->SetNumberOfWorkUnits(4);
    for (itk::IndexValueType i = 0; i < 4; ++i)
    {
      dummySrc->SetExceptionIndex(i);
      ITK_TRY_EXPECT_EXCEPTION(dummySrc->Update());
    }
  }
  std::cout << "Test PASSED!" << std::endl;
  return EXIT_SUCCESS;
}
