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

#include "itkTobogganImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTextOutput.h"


int itkTobogganImageFilterTest(int ac, char* av[] )
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " InputImage OutputImage\n";
    exit ( 1 );
    }


  const unsigned int Dimension = 2;
  typedef unsigned char                               PixelType;
  typedef itk::Image<PixelType, Dimension>            InputImageType;
  typedef itk::Image<float, Dimension>                FloatImageType;
  typedef itk::Image<PixelType, Dimension>            OutputImageType;
  typedef itk::Image< itk::IdentifierType, Dimension> LongImageType;


  // Create a pipeline
  typedef itk::CastImageFilter<InputImageType, FloatImageType>
                                           InCastType;
  typedef itk::TobogganImageFilter<FloatImageType>
                                           FilterType;
  typedef itk::CastImageFilter<LongImageType, OutputImageType>
                                           CastType;
  typedef itk::GradientMagnitudeRecursiveGaussianImageFilter<FloatImageType,FloatImageType>
                                           GMGaussianType;

  FilterType::Pointer toboggan = FilterType::New();
  CastType::Pointer cast = CastType::New();
  InCastType::Pointer incast = InCastType::New();
  GMGaussianType::Pointer gmgaussian = GMGaussianType::New();

  itk::ImageFileReader<InputImageType>::Pointer input
    = itk::ImageFileReader<InputImageType>::New();

  input->SetFileName(av[1]);
  incast->SetInput ( input->GetOutput() );
  gmgaussian->SetInput ( incast->GetOutput() );
  gmgaussian->SetSigma ( 15.0 );
  toboggan->SetInput ( gmgaussian->GetOutput() );
  cast->SetInput ( toboggan->GetOutput() );
  try
    {
    input->Update();
    cast->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    exit ( 1 );
    }

  itk::ImageFileWriter<OutputImageType>::Pointer writer;
  writer = itk::ImageFileWriter<OutputImageType>::New();
  writer->SetInput( cast->GetOutput() );
  writer->SetFileName( av[2] );
  writer->Update();

  return EXIT_SUCCESS;
}
