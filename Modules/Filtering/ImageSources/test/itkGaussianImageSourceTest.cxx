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

#include "itkFilterWatcher.h"
#include "itkGaussianImageSource.h"
#include "itkTestingMacros.h"
#include "itkMath.h"

int itkGaussianImageSourceTest(int, char* [] )
{
  // This can be changed!
  const unsigned int    Dimension = 3;
  typedef unsigned char PixelType;

  // Image typedef
  typedef itk::Image< PixelType, Dimension > ImageType;

  // Create a gaussian image source
  typedef itk::GaussianImageSource< ImageType > GaussianSourceType;
  GaussianSourceType::Pointer source = GaussianSourceType::New();
  FilterWatcher watcher(source, "source");

  ImageType::SpacingValueType   spacing[] = { 1.2f, 1.3f, 1.4f };
  ImageType::PointValueType     origin[] = { 1.0f, 4.0f, 2.0f };
  ImageType::SizeValueType      size[] = {  130,  150,  120 };

  GaussianSourceType::ArrayType mean;
  mean[0] = size[0]/2.0f + origin[0];
  mean[1] = size[1]/2.0f + origin[1];
  mean[2] = size[2]/2.0f + origin[2];

  GaussianSourceType::ArrayType sigma;
  sigma[0] = 25.0f;
  sigma[1] = 35.0f;
  sigma[2] = 55.0f;

  source->SetSize( size );
  source->SetOrigin( origin );
  source->SetSpacing( spacing );
  source->SetMean( mean );
  source->SetSigma( sigma );

  // Test the get macros as well (booorrring...)
  source->GetSize();
  source->GetSpacing();
  source->GetOrigin();
  source->GetDirection();
  source->GetScale();
  source->GetNormalized();
  source->GetSigma();
  source->GetMean();

  // Test the get/set parameters
  GaussianSourceType::ParametersType params = source->GetParameters();
  if ( params.GetSize() != 7 )
    {
    std::cerr << "Incorrect number of parameters. Expected 7, got "
              << params.GetSize() << "." << std::endl;
    return EXIT_FAILURE;
    }

  if ( itk::Math::NotAlmostEquals( params[0], sigma[0] ) ||
       itk::Math::NotAlmostEquals( params[1], sigma[1] ) ||
       itk::Math::NotAlmostEquals( params[2], sigma[2] ) )
    {
    std::cerr << "Parameters have incorrect sigma value." << std::endl;
    return EXIT_FAILURE;
    }

  if ( itk::Math::NotAlmostEquals( params[3], mean[0] ) ||
       itk::Math::NotAlmostEquals( params[4], mean[1] ) ||
       itk::Math::NotAlmostEquals( params[5], mean[2] ) )
    {
    std::cerr << "Parameters have incorrect mean value." << std::endl;
    return EXIT_FAILURE;
    }

  if ( itk::Math::NotAlmostEquals( params[6], source->GetScale() ) )
    {
    std::cerr << "Parameters have incorrect scale value." << std::endl;
    return EXIT_FAILURE;
    }

  params[0] = 12.0;
  params[1] = 13.0;
  params[2] = 14.0;
  params[3] = 22.0;
  params[4] = 32.0;
  params[5] = 42.0;
  params[6] = 55.5;
  source->SetParameters( params );

  if ( itk::Math::NotAlmostEquals( source->GetSigma()[0], params[0] ) ||
       itk::Math::NotAlmostEquals( source->GetSigma()[1], params[1] ) ||
       itk::Math::NotAlmostEquals( source->GetSigma()[2], params[2] ) )
    {
    std::cerr << "Sigma disagrees with parameters array." << std::endl;
    std::cerr << "Sigma: " << source->GetSigma() << ", parameters: ["
              << params[0] << ", " << params[1] << ", " << params[2]
              << "]" << std::endl;
    return EXIT_FAILURE;
    }

  if ( itk::Math::NotAlmostEquals( source->GetMean()[0], params[3] ) ||
       itk::Math::NotAlmostEquals( source->GetMean()[1], params[4] ) ||
       itk::Math::NotAlmostEquals( source->GetMean()[2], params[5] ) )
    {
    std::cerr << "Mean disagrees with parameters array." << std::endl;
    std::cerr << "Mean: " << source->GetMean() << ", parameters: ["
             << params[3] << ", " << params[4] << ", " << params[5]
             << "]" << std::endl;
    return EXIT_FAILURE;
    }

  if ( itk::Math::NotAlmostEquals( source->GetScale(), params[6] ) )
    {
    std::cerr << "Scale disagrees with parameters array." << std::endl;
    std::cerr << "Scale: " << source->GetScale() << ", parameters: "
              << params[6] << std::endl;
    return EXIT_FAILURE;
    }

  // Get the output of the source
  ImageType::ConstPointer image = source->GetOutput();

  // Run the pipeline
  TRY_EXPECT_NO_EXCEPTION( source->Update() );

  // Exercise the print method
  std::cout << image << std::endl;

  // Instantiate 1D case.
  typedef itk::Image< PixelType, 1 >              Image1DType;
  typedef itk::GaussianImageSource< Image1DType > GaussianSource1DType;
  GaussianSource1DType::Pointer source1D = GaussianSource1DType::New();

  return EXIT_SUCCESS;
}
