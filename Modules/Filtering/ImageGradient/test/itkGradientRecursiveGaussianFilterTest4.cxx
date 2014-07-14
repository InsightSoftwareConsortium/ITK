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

#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


int itkGradientRecursiveGaussianFilterTest4(int argc, char* argv[] )
{

  if ( argc != 3 )
    {
    std::cerr << "Missing Parameters!" << std::endl;
    std::cerr << " inputImageFile outputImageFile" << std::endl;
    return EXIT_FAILURE;
    }

  std::string inFileName = argv[1];

  std::string outFileName = argv[2];

  // Define the dimension of the images
  const unsigned int myDimension = 2;

  // Declare the types of the images
  typedef itk::Image<float, myDimension>       myImageType;

  // Create the image
  myImageType::Pointer inputImage  = myImageType::New();


  typedef itk::ImageFileReader<myImageType> myReaderType;
  myReaderType::Pointer reader = myReaderType::New();
  reader->SetFileName( inFileName );


  typedef itk::VectorImage< float, myDimension> myGradientImageType;


  // Declare the type for the
  typedef itk::GradientRecursiveGaussianImageFilter< myImageType, myGradientImageType >  myFilterType;


  // Create a  Filter
  myFilterType::Pointer filter = myFilterType::New();


  // Connect the input images
  filter->SetInput( reader->GetOutput() );

  // Select the value of Sigma
  filter->SetSigma( 2.5 );


  typedef itk::ImageFileWriter<myGradientImageType> myWriterType;
  myWriterType::Pointer writer = myWriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( outFileName );
  writer->Update();

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
