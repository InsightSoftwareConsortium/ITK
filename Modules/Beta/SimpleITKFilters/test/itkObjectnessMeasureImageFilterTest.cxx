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

#include "itkTestingMacros.h"
#include "itkFilterWatcher.h"
#include "itkObjectnessMeasureImageFilter.h"
#include "itkSmoothingRecursiveGaussianImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int itkObjectnessMeasureImageFilterTest(int argc, char *argv[])
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " inputImage outputImage [ObjectDimension] [Bright/Dark]" << std::endl;
    return EXIT_FAILURE;
    }
  const char *inputImageFileName = argv[1];
  const char *outputImageFileName  = argv[2];

  const unsigned int objectDimension = (argc >= 3) ? atoi(argv[3]) : 3;
  const bool brightObject = (argc >= 4) ? atoi(argv[4]) : true;
  const double alphaValue = 0.5;
  const double betaValue = 0.5;
  const double gammaValue = 0.5;

  const unsigned int Dimension = 2;
  typedef double                             PixelType;
  typedef itk::Image< PixelType, Dimension > ImageType;

  typedef itk::ImageFileReader<ImageType> ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( inputImageFileName );

  typedef itk::SmoothingRecursiveGaussianImageFilter<ImageType, ImageType> SmoothingFilterType;
  SmoothingFilterType::Pointer smoothing = SmoothingFilterType::New();
  smoothing->SetSigma(1.0);
  smoothing->SetInput(reader->GetOutput());

  typedef itk::ObjectnessMeasureImageFilter<ImageType, ImageType> FilterType;
  FilterType::Pointer filter =  FilterType::New();
  filter->SetInput(smoothing->GetOutput());
  filter->SetAlpha(alphaValue);
  filter->SetBeta(betaValue);
  filter->SetGamma(gammaValue);
  filter->SetBrightObject(brightObject);
  filter->SetObjectDimension(objectDimension);
  filter->SetScaleObjectnessMeasure(false);

  FilterWatcher watcher(filter);

  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName( outputImageFileName );
  writer->Update();

  //EXERCISE_BASIC_OBJECT_METHODS( FilterType, ObjectnessMeasureImageFilter );

  return EXIT_SUCCESS;
}
