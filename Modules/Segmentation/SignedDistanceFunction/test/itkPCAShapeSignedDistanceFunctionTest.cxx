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

#include "itkPCAShapeSignedDistanceFunction.h"

#include "vnl/vnl_sample.h"
#include "itkImageRegionIterator.h"
#include "itkEuler2DTransform.h"


/**
 * This module tests the functionality of the PCAShapeSignedDistanceFunction
 * class.
 *
 * The mean image, principal component images, standard deviations, and
 * and weighting parameters are randomly generated. The signed distance is
 * evaluated at all image points and compared to expected values.
 * The test fails if the evaluated results is not within a certain tolerance
 * of the expected results.
 */
int itkPCAShapeSignedDistanceFunctionTest( int, char *[])
{
  unsigned int i;
  typedef double CoordRep;
  const unsigned int Dimension    = 2;
  const unsigned int ImageWidth   = 3;
  const unsigned int ImageHeight  = 2;
  const unsigned int NumberOfPCs  = 3;


  // define a pca shape function
  typedef itk::PCAShapeSignedDistanceFunction<CoordRep,Dimension> ShapeFunction;
  ShapeFunction::Pointer shape = ShapeFunction::New();
//  shape->DebugOn();
  shape->SetNumberOfPrincipalComponents(NumberOfPCs);


  // set up the transform
  typedef itk::Euler2DTransform<double> transformType;
  transformType::Pointer transform = transformType::New();
  shape->SetTransform(transform);


  // prepare for image creation
  typedef ShapeFunction::ImageType ImageType;

  ImageType::SizeType imageSize = {{ImageWidth, ImageHeight}};

  ImageType::IndexType startIndex;
  startIndex.Fill(0);

  ImageType::RegionType  region;
  region.SetSize(imageSize);
  region.SetIndex(startIndex);


  // set up the random number generator
  vnl_sample_reseed();
  ImageType::PixelType randomPixel;


  // set up the mean image
  ImageType::Pointer meanImage = ImageType::New();
  meanImage->SetRegions(region);
  meanImage->Allocate();

  typedef itk::ImageRegionIterator<ImageType> ImageIterator;
  ImageIterator meanImageIt(meanImage, meanImage->GetBufferedRegion());

  for(meanImageIt.GoToBegin(); !meanImageIt.IsAtEnd(); ++meanImageIt)
    {
    randomPixel = vnl_sample_normal(0, 1);
    meanImageIt.Set(randomPixel);
    }

  shape->SetMeanImage(meanImage);


  // set up the NumberOfPCs principal component images
  ShapeFunction::ImagePointerVector   pcImages(NumberOfPCs);
  typedef std::vector<ImageIterator>  ImageIteratorVector;
  ImageIteratorVector                 pcImageIts(NumberOfPCs);

  for(i=0; i<NumberOfPCs; i++)
    {
    pcImages[i] = ImageType::New();
    pcImages[i]->SetRegions(region);
    pcImages[i]->Allocate();

    pcImageIts[i] = ImageIterator(pcImages[i], pcImages[i]->GetBufferedRegion());

    for(pcImageIts[i].GoToBegin(); !pcImageIts[i].IsAtEnd(); ++pcImageIts[i])
      {
      randomPixel = vnl_sample_normal(0, 1);
      pcImageIts[i].Set(randomPixel);
      }
    }

  shape->SetPrincipalComponentImages(pcImages);


  // set up the standard deviation for each principal component images
  ShapeFunction::ParametersType pcStandardDeviations(NumberOfPCs);

  for(i=0; i<NumberOfPCs; i++)
    { pcStandardDeviations[i] = vnl_sample_normal(0, 1); }

  shape->SetPrincipalComponentStandardDeviations(pcStandardDeviations);


  // set up the parameters
  unsigned int numberOfShapeParameters  = shape->GetNumberOfShapeParameters();
  unsigned int numberOfPoseParameters   = shape->GetNumberOfPoseParameters();
  unsigned int numberOfParameters =
    numberOfShapeParameters  + numberOfPoseParameters;
  ShapeFunction::ParametersType parameters(numberOfParameters);

  for(i=0; i<numberOfParameters; i++)
    { parameters[i] = vnl_sample_normal(0, 1); }

  shape->SetParameters(parameters);


  // we must initialize the function before use
  shape->Initialize();

  // check pca shape calculation
  ShapeFunction::PointType  point;
  ImageType::IndexType      index;
  ShapeFunction::OutputType output;
  ShapeFunction::OutputType expected;

  std::cout << "check results:" << std::endl;
  unsigned int numberOfRotationParameters = Dimension*(Dimension-1)/2;
  unsigned int startIndexOfTranslationParameters =
    numberOfShapeParameters + numberOfRotationParameters;

  ShapeFunction::TransformType::InputPointType p;
  ShapeFunction::TransformType::InputPointType q;

  for(meanImageIt.GoToBegin(); !meanImageIt.IsAtEnd(); ++meanImageIt)
    {
    // from index to physical point
    index = meanImageIt.GetIndex();
    meanImage->TransformIndexToPhysicalPoint(index, point);

    // inverse Euler2DTransform: first translation then rotation
    p[0] =  point[0] - parameters[startIndexOfTranslationParameters];
    p[1] =  point[1] - parameters[startIndexOfTranslationParameters + 1];

    double angle = parameters[numberOfShapeParameters];
    q[0] =  p[0] * std::cos(-angle) - p[1] * std::sin(-angle);
    q[1] =  p[0] * std::sin(-angle) + p[1] * std::cos(-angle);

    // evaluate shape function
    output = shape->Evaluate(q);

    // calculate expected function value
    expected = meanImage->GetPixel(index);
    for(i=0; i<NumberOfPCs; i++)
      {
      expected += pcImages[i]->GetPixel(index) *
      pcStandardDeviations[i] *
      parameters[i];
      }

    // check result
    std::cout << "f(" << point << ") = " << output << std::endl;

    if(itk::Math::abs( output - expected ) > 1e-9)
      {
      std::cout << "But expected value is: " << expected << std::endl;
      return EXIT_FAILURE;
      }
    }

 // Evaluate at a point outside the image domain
 std::cout << "Evaluate at point outside image domain" << std::endl;
 q.Fill( 5.0 );
 output = shape->Evaluate( q );
 std::cout << "f(" << q << ") = " << output << std::endl;

 // Exercise other methods for test coverage
 shape->Print( std::cout );

 std::cout << "NumberOfPrincipalComponents: "
           << shape->GetNumberOfPrincipalComponents() << std::endl;
 std::cout << "MeanImage: "
           << shape->GetMeanImage() << std::endl;
 std::cout << "PrincipalComponentStandardDeviations: "
           << shape->GetPrincipalComponentStandardDeviations() << std::endl;
 std::cout << "Transform: "
           << shape->GetTransform() << std::endl;
 std::cout << "Parameters: "
           << shape->GetParameters() << std::endl;

  // Exercise error testing
  bool pass;

#define TEST_INITIALIZATION_ERROR( ComponentName, badComponent, goodComponent ) \
  shape->Set##ComponentName( badComponent ); \
  try \
    { \
    pass = false; \
    shape->Initialize(); \
    } \
  catch( itk::ExceptionObject& err ) \
    { \
    std::cout << "Caught expected ExceptionObject" << std::endl; \
    std::cout << err << std::endl; \
    pass = true; \
    } \
  shape->Set##ComponentName( goodComponent ); \
  \
  if( !pass ) \
    { \
    std::cout << "Test failed." << std::endl; \
    return EXIT_FAILURE; \
    }

  // ITK_NULLPTR MeanImage
  TEST_INITIALIZATION_ERROR( MeanImage, ITK_NULLPTR, meanImage );

  // Wrong number of PC images
  ShapeFunction::ImagePointerVector   badPCImages;
  badPCImages.resize(1);
  badPCImages[0] = ITK_NULLPTR;

  TEST_INITIALIZATION_ERROR( PrincipalComponentImages, badPCImages, pcImages );

  // A ITK_NULLPTR PC image
  badPCImages = pcImages;
  badPCImages[1] = ITK_NULLPTR;

  TEST_INITIALIZATION_ERROR( PrincipalComponentImages, badPCImages, pcImages );

  // A PC image of the wrong size
  ImageType::SizeType badSize;
  badSize.Fill( 1 );
  ImageType::RegionType badRegion( badSize );
  badPCImages[1] = ImageType::New();
  badPCImages[1]->SetRegions( badRegion );
  badPCImages[1]->Allocate();
  badPCImages[1]->FillBuffer( 0.0 );

  TEST_INITIALIZATION_ERROR( PrincipalComponentImages, badPCImages, pcImages );

  std::cout << "Test passed. " << std::endl;
  return EXIT_SUCCESS;
}
