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

#include <fstream>
#include "itkVectorGradientMagnitudeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"


int itkVectorGradientMagnitudeImageFilterTest2(int ac, char* av[] )
{
  typedef itk::RGBPixel<unsigned char>                          RGBPixelType;
  typedef itk::Image<RGBPixelType, 3>                           RGBImageType;
  typedef itk::Image<unsigned char, 3>                          CharImage3Type;
  typedef itk::Image<unsigned char, 2>                          CharImage2Type;
  typedef itk::VectorGradientMagnitudeImageFilter<RGBImageType> FilterType;
  typedef itk::ImageFileReader<RGBImageType>                    ReaderType;
  typedef itk::RescaleIntensityImageFilter<FilterType::OutputImageType, CharImage3Type>
                                                                RescaleFilterType;
  typedef itk::ImageFileWriter<CharImage2Type>                  WriterType;


  if(ac < 5)
    {
    std::cerr << "Usage: " << av[0] << " InputImage OutputImage Mode SliceToExtract\n";
    return EXIT_FAILURE;
    }

  // Create a reader and filter
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(av[1]);
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(reader->GetOutput());

  const int mode = ::atoi( av[3] );

  if ( mode == 1)
    {
    filter->SetUsePrincipleComponentsOn();
    }
  else
    {
    filter->SetUsePrincipleComponentsOff();
    }

  RescaleFilterType::Pointer rescale = RescaleFilterType::New();
  rescale->SetOutputMinimum(0);
  rescale->SetOutputMaximum(255);
  rescale->SetInput( filter->GetOutput() );

  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( av[2] );

  try
    {
    rescale->Update();

    // Extract one slice to write for regression testing
    CharImage3Type::RegionType extractedRegion = rescale->GetOutput()->GetRequestedRegion();
    extractedRegion.SetSize(2,1);
    extractedRegion.SetIndex(2,::atoi(av[4]));

    CharImage2Type::Pointer extractedImage = CharImage2Type::New();
    CharImage2Type::RegionType reg;
    reg.SetSize(0,extractedRegion.GetSize()[0]);
    reg.SetSize(1,extractedRegion.GetSize()[1]);
    reg.SetIndex(0,0);
    reg.SetIndex(1,0);
    extractedImage->SetRegions(reg);
    extractedImage->Allocate();
    double sp[2];
    sp[0] = rescale->GetOutput()->GetSpacing()[0];
    sp[1] = rescale->GetOutput()->GetSpacing()[1];
    extractedImage->SetSpacing(sp);

    itk::ImageRegionIterator<CharImage3Type> in(rescale->GetOutput(), extractedRegion);
    itk::ImageRegionIterator<CharImage2Type> out(extractedImage,
                                                 extractedImage->GetRequestedRegion());

    for (; !in.IsAtEnd(); ++in, ++out)
      {
      out.Set(in.Get());
      }

    writer->SetInput( extractedImage );
    writer->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return EXIT_FAILURE;
    }
  catch (...)
    {
    std::cerr << "Some other exception occurred" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout <<  "The gradient image range was (low, high) = ("
            <<  rescale->GetInputMinimum() << ", " << rescale->GetInputMaximum()
            << ")" << std::endl;
  std::cout <<  "Output was scaled, shifted = " << rescale->GetScale() << ", "
            << rescale->GetShift() << std::endl;

  return EXIT_SUCCESS;
}
