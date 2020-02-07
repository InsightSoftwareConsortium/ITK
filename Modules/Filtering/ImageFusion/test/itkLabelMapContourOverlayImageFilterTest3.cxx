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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkLabelMapContourOverlayImageFilter.h"


int
itkLabelMapContourOverlayImageFilterTest3(int argc, char * argv[])
{
  if (argc != 9)
  {
    std::cerr << "usage: " << argv[0] << " input input output opacity type thickness dilation priority sliceDim"
              << std::endl;
    return EXIT_FAILURE;
  }

  constexpr int Dimension = 2;

  using ImageType = itk::Image<unsigned char, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using ConverterType = itk::LabelImageToLabelMapFilter<ImageType>;
  ConverterType::Pointer converter = ConverterType::New();
  converter->SetInput(reader->GetOutput());

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);

  using ColorizerType = itk::LabelMapContourOverlayImageFilter<ConverterType::OutputImageType, ImageType>;
  ColorizerType::Pointer colorizer = ColorizerType::New();
  colorizer->SetInput(converter->GetOutput());
  colorizer->SetFeatureImage(reader2->GetOutput());
  colorizer->SetOpacity(std::stod(argv[4]));
  colorizer->SetType(std::stoi(argv[5]));
  ColorizerType::SizeType r;
  r.Fill(std::stoi(argv[6]));
  colorizer->SetContourThickness(r);
  r.Fill(std::stoi(argv[7]));
  colorizer->SetDilationRadius(r);
  colorizer->SetPriority(std::stoi(argv[8]));

  // Replace colormap with a custom one.
  // Just cycle through three colors for this test.
  ColorizerType::FunctorType functor;
  functor.ResetColors();
  functor.AddColor(0, 0, 255);
  functor.AddColor(0, 255, 0);
  functor.AddColor(255, 0, 0);
  colorizer->SetFunctor(functor);

  itk::SimpleFilterWatcher watcher(colorizer, "filter");

  using WriterType = itk::ImageFileWriter<ColorizerType::OutputImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(colorizer->GetOutput());
  writer->SetFileName(argv[3]);
  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "Unexpected exception." << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }
  return 0;
}
