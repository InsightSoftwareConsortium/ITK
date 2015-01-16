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
#include "itkHDF5ImageIOFactory.h"
#include "itkIOTestHelper.h"
#include "itkPipelineMonitorImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkImageDuplicator.h"

template <typename TPixel>
int HDF5ReadWriteTest2(const char *fileName)
{
  int success(EXIT_SUCCESS);
  typedef typename itk::Image<TPixel,3> ImageType;
  typename ImageType::RegionType imageRegion;
  typename ImageType::SizeType size;
  typename ImageType::IndexType index;
  typename ImageType::SpacingType spacing;
  typename ImageType::PointType origin;
  typename ImageType::DirectionType myDirection;
  for(unsigned i = 0; i < 3; i++)
    {
    size[i] = 5;
    index[i] = 0;
    spacing[i] = 1.0 / (static_cast<double>(i) + 1.0);
    origin[i] = static_cast<double>(i) + 7.0;
    }
  imageRegion.SetSize(size);
  imageRegion.SetIndex(index);
  typename ImageType::Pointer im =
    itk::IOTestHelper::AllocateImageFromRegionAndSpacing<ImageType>(imageRegion,spacing);
  //
  // fill image buffer
  vnl_random randgen(12345678);
  itk::ImageRegionIterator<ImageType> it(im,im->GetLargestPossibleRegion());
  for(it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    TPixel pix;
    itk::IOTestHelper::RandomPix(randgen,pix);
    it.Set(pix);
    }

  // Duplicate im into clonedImage, because:
  // - clonedImage will be lost, because PipelineMonitorImageFilter always run
  //   in-place and will use im to store its output.
  // - im is used latter to check values of the written-then-read image.
  typedef typename itk::ImageDuplicator< ImageType > DuplicatorType;
  typename DuplicatorType::Pointer duplicator = DuplicatorType::New();
  duplicator->SetInputImage(im);
  duplicator->Update();
  typename ImageType::Pointer clonedImage = duplicator->GetModifiableOutput();

  typedef typename itk::ImageFileWriter<ImageType> WriterType;
  typename WriterType::Pointer writer = WriterType::New();

  typedef typename itk::PipelineMonitorImageFilter<ImageType> MonitorFilterType;
  typename MonitorFilterType::Pointer writerMonitor = MonitorFilterType::New();
  writerMonitor->SetInput(clonedImage);

  writer->SetFileName(fileName);
  writer->SetInput(writerMonitor->GetOutput());
  writer->SetNumberOfStreamDivisions(5);
  try
    {
    writer->Write();
    }
  catch(itk::ExceptionObject &err)
    {
    std::cout << "itkHDF5ImageIOTest" << std::endl
              << "Exception Object caught: " << std::endl
              << err << std::endl;
    return EXIT_FAILURE;
    }

  // Check streaming regions.
  if (! writerMonitor->VerifyInputFilterExecutedStreaming(5))
    success = EXIT_FAILURE;
  // force writer close
  writer = typename WriterType::Pointer();

  // Read image with streaming.
  typedef typename itk::ImageFileReader<ImageType> ReaderType;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(fileName);
  reader->SetUseStreaming(true);
  typename MonitorFilterType::Pointer readerMonitor = MonitorFilterType::New();
  readerMonitor->SetInput(reader->GetOutput());
  typedef typename itk::StreamingImageFilter<ImageType, ImageType> StreamingFilter;
  typename StreamingFilter::Pointer streamer = StreamingFilter::New();
  streamer->SetInput(readerMonitor->GetOutput());
  streamer->SetNumberOfStreamDivisions(5);
  typename ImageType::Pointer im2;
  try
    {
    streamer->Update();
    }
  catch(itk::ExceptionObject &err)
    {
    std::cout << "itkHDF5ImageIOTest" << std::endl
              << "Exception Object caught: " << std::endl
              << err << std::endl;
    return EXIT_FAILURE;
    }
  im2 = streamer->GetOutput();
  itk::ImageRegionIterator<ImageType> it2(im2,im2->GetLargestPossibleRegion());
  for(it.GoToBegin(),it2.GoToBegin(); !it.IsAtEnd() && !it2.IsAtEnd(); ++it,++it2)
    {
    if(it.Value() != it2.Value())
      {
      std::cout << "Original Pixel (" << it.Value()
                << ") doesn't match read-in Pixel ("
                << it2.Value() << std::endl;
      success = EXIT_FAILURE;
      break;
      }
    }

  // Check number of streaming regions.
  if (! readerMonitor->VerifyInputFilterExecutedStreaming(5))
    return EXIT_FAILURE;
  itk::IOTestHelper::Remove(fileName);
  return success;
}

int
itkHDF5ImageIOStreamingReadWriteTest(int ac, char * av [])
{
  std::string prefix("");
  if(ac > 1)
    {
    prefix = *++av;
    --ac;
    itksys::SystemTools::ChangeDirectory(prefix.c_str());
    }
  itk::ObjectFactoryBase::RegisterFactory(itk::HDF5ImageIOFactory::New() );

  int result(0);
  result += HDF5ReadWriteTest2<unsigned char>("StreamingUCharImage.hdf5");
  result += HDF5ReadWriteTest2<float>("StreamingFloatImage.hdf5");
  result += HDF5ReadWriteTest2<itk::RGBPixel<unsigned char> >("StreamingRGBImage.hdf5");
  return result != 0;
}
