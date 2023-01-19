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
#include "itkHDF5ImageIOFactory.h"
#include "itkIOTestHelper.h"
#include "itkPipelineMonitorImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkGenerateImageSource.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageDuplicator.h"
#include "itkMath.h"

namespace itk
{
/**
 * \class DemoImageSource
 *
 * \brief Streamable process that will generate image regions from the write requests
 *
 * We do not allocate directly the Image, because as an Image is a data and is not streamable,
 * the writer would write the image in one pass, without streaming.
 *
 * Instead, we need to set a streamable source process as the writer input.
 * This source process, 'DemoImageSource', will allocate a region of the image
 * (and set pixels values) on the fly, based on the informations
 * received from the writer requests.
 */
template <class TOutputImage>
class DemoImageSource : public GenerateImageSource<TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DemoImageSource);

  /** Standard class type aliases. */
  using Self = DemoImageSource;
  using Superclass = DemoImageSource<TOutputImage>;
  using Pointer = SmartPointer<Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DemoImageSource, GenerateImageSource);

  /** Set the value to fill the image. */
  itkSetMacro(Value, typename TOutputImage::PixelType);

protected:
  DemoImageSource() { m_Value = NumericTraits<typename TOutputImage::PixelType>::ZeroValue(); }
  ~DemoImageSource() override = default;

  /** Does the real work. */
  void
  GenerateData() override
  {
    TOutputImage * out = this->GetOutput();
    out->SetBufferedRegion(out->GetRequestedRegion());
    out->Allocate();
    itk::ImageRegionIteratorWithIndex<TOutputImage> it(out, out->GetRequestedRegion());
    for (it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
      typename TOutputImage::IndexType       idx = it.GetIndex();
      const typename TOutputImage::PixelType pixel(idx[2] * 100 + idx[1] * 10 + idx[0]);
      it.Set(pixel);
    }
  };

private:
  typename TOutputImage::PixelType m_Value;
};

} // namespace itk

template <typename TPixel>
int
HDF5ReadWriteTest2(const char * fileName)
{
  // Define image type.
  using ImageType = typename itk::Image<TPixel, 3>;

  // Create a source object (in this case a constant image).
  typename ImageType::SizeType size;
  size[2] = 5;
  size[1] = 5;
  size[0] = 5;
  typename itk::DemoImageSource<ImageType>::Pointer imageSource = itk::DemoImageSource<ImageType>::New();
  imageSource->SetValue(static_cast<TPixel>(23)); // Not used.
  imageSource->SetSize(size);

  // Write image with streaming.
  using WriterType = typename itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  using MonitorFilterType = typename itk::PipelineMonitorImageFilter<ImageType>;
  auto writerMonitor = MonitorFilterType::New();
  writerMonitor->SetInput(imageSource->GetOutput());
  writer->SetFileName(fileName);
  writer->SetInput(writerMonitor->GetOutput());
  writer->SetNumberOfStreamDivisions(5);
  try
  {
    writer->Write();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "itkHDF5ImageIOTest" << std::endl << "Exception Object caught: " << std::endl << err << std::endl;
    return EXIT_FAILURE;
  }

  // Check streaming regions.
  if (!writerMonitor->VerifyInputFilterExecutedStreaming(5))
  {
    return EXIT_FAILURE;
  }
  typename ImageType::RegionType expectedRegion;
  expectedRegion.SetIndex(0, 0);
  expectedRegion.SetIndex(1, 0);
  expectedRegion.SetSize(0, 5);
  expectedRegion.SetSize(1, 5);
  expectedRegion.SetSize(2, 1);
  typename MonitorFilterType::RegionVectorType   writerRegionVector = writerMonitor->GetUpdatedBufferedRegions();
  typename ImageType::RegionType::IndexValueType iRegion;
  for (iRegion = 0; iRegion < 5; ++iRegion)
  {
    expectedRegion.SetIndex(2, iRegion);
    if (writerRegionVector[iRegion] != expectedRegion)
    {
      std::cout << "Written image region number " << iRegion << " :" << writerRegionVector[iRegion]
                << " doesn't match expected one: " << expectedRegion << std::endl;
      return EXIT_FAILURE;
    }
  }

  // Force writer close.
  writer = typename WriterType::Pointer();

  // Read image with streaming.
  using ReaderType = typename itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(fileName);
  reader->SetUseStreaming(true);
  auto readerMonitor = MonitorFilterType::New();
  readerMonitor->SetInput(reader->GetOutput());
  using StreamingFilter = typename itk::StreamingImageFilter<ImageType, ImageType>;
  auto streamer = StreamingFilter::New();
  streamer->SetInput(readerMonitor->GetOutput());
  streamer->SetNumberOfStreamDivisions(5);
  typename ImageType::Pointer image;
  try
  {
    streamer->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "itkHDF5ImageIOTest" << std::endl << "Exception Object caught: " << std::endl << err << std::endl;
    return EXIT_FAILURE;
  }
  image = streamer->GetOutput();

  // Check largest possible and buffered regions.
  expectedRegion.SetIndex(0, 0);
  expectedRegion.SetIndex(1, 0);
  expectedRegion.SetIndex(2, 0);
  expectedRegion.SetSize(0, 5);
  expectedRegion.SetSize(1, 5);
  expectedRegion.SetSize(2, 5);
  if (image->GetLargestPossibleRegion() != expectedRegion)
  {
    std::cout << "Read image largest possible region: " << image->GetLargestPossibleRegion()
              << "n doesn't match expectedo one: " << expectedRegion << std::endl;
  }
  if (image->GetBufferedRegion() != expectedRegion)
  {
    std::cout << "Read image buffered region: " << image->GetBufferedRegion()
              << "n doesn't match expectedo one: " << expectedRegion << std::endl;
  }

  // Check image pixel values.
  itk::ImageRegionIterator<ImageType> it(image, image->GetLargestPossibleRegion());
  typename ImageType::IndexType       idx;
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    idx = it.GetIndex();
    const TPixel origValue(idx[2] * 100 + idx[1] * 10 + idx[0]);
    if (itk::Math::NotAlmostEquals(it.Get(), origValue))
    {
      std::cout << "Original Pixel (" << origValue << ") doesn't match read-in Pixel (" << it.Get() << ")" << std::endl;
      return EXIT_FAILURE;
    }
  }

  // Check number of streaming regions.
  if (!readerMonitor->VerifyInputFilterExecutedStreaming(5))
  {
    return EXIT_FAILURE;
  }

  // Check streaming regions.
  typename MonitorFilterType::RegionVectorType readerRegionVector = readerMonitor->GetUpdatedBufferedRegions();
  expectedRegion.SetIndex(0, 0);
  expectedRegion.SetIndex(1, 0);
  expectedRegion.SetSize(0, 5);
  expectedRegion.SetSize(1, 5);
  expectedRegion.SetSize(2, 1);
  for (iRegion = 0; iRegion < 5; ++iRegion)
  {
    expectedRegion.SetIndex(2, iRegion);
    if (readerRegionVector[iRegion] != expectedRegion)
    {
      std::cout << "Read image region number " << iRegion << " :" << readerRegionVector[iRegion]
                << " doesn't match expected one: " << expectedRegion << std::endl;
      return EXIT_FAILURE;
    }
  }

  // Clean working directory.
  itk::IOTestHelper::Remove(fileName);

  return EXIT_SUCCESS;
}

int
itkHDF5ImageIOStreamingReadWriteTest(int argc, char * argv[])
{
  std::string prefix("");
  if (argc > 1)
  {
    prefix = *++argv;
    --argc;
    itksys::SystemTools::ChangeDirectory(prefix.c_str());
  }
  itk::ObjectFactoryBase::RegisterFactory(itk::HDF5ImageIOFactory::New());

  int result(0);
  result += HDF5ReadWriteTest2<unsigned char>("StreamingUCharImage.hdf5");
  result += HDF5ReadWriteTest2<float>("StreamingFloatImage.hdf5");
  result += HDF5ReadWriteTest2<itk::RGBPixel<unsigned char>>("StreamingRGBImage.hdf5");
  return result != 0;
}
