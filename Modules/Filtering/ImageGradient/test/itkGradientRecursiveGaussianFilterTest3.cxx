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
#include "itkVector.h"
#include "itkVariableLengthVector.h"
#include "itkVectorImage.h"
#include "itkImageFileWriter.h"

/*
 * Test itkGradientRecursiveGaussianFilter with various types
 * of images of vector pixels, and with VectorImage.
 * Some results validation testing is done directly below.
 * The output of the various types with same pixel length are compared via ctest, to
 * verify they're the same.
 */

template< typename TImageType, typename TGradImageType, unsigned int TComponents >
int itkGradientRecursiveGaussianFilterTest3Run( typename TImageType::PixelType & myPixelBorder,
                                                typename TImageType::PixelType & myPixelFill,
                                                typename TGradImageType::Pointer & outputImage, char * outputFilename )
{
  typedef TImageType      myImageType;
  typedef TGradImageType  myGradImageType;

  const unsigned int myComponents = TComponents;

  // Define the dimension of the images
  const unsigned int myDimension = myImageType::ImageDimension;

  // Declare the type of the index to access images
  typedef itk::Index<myDimension>             myIndexType;

  // Declare the type of the size
  typedef itk::Size<myDimension>              mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension>        myRegionType;

  // Create the image
  typename myImageType::Pointer inputImage  = myImageType::New();

  // Define their size, and start index
  mySizeType size;
  size[0] = 8;
  size[1] = 8;
  size[2] = 8;

  myIndexType start;
  start.Fill(0);

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->SetNumberOfComponentsPerPixel( myComponents );
  inputImage->Allocate();

  // Declare Iterator type for the input image
  typedef itk::ImageRegionIteratorWithIndex<myImageType>  myIteratorType;

  // Create one iterator for the Input Image A (this is a light object)
  myIteratorType it( inputImage, inputImage->GetRequestedRegion() );

  // Initialize the content of Image A
  while( !it.IsAtEnd() )
    {
    it.Set( myPixelBorder );
    ++it;
    }

  size[0] = 4;
  size[1] = 4;
  size[2] = 4;

  start[0] = 2;
  start[1] = 2;
  start[2] = 2;

  // Create one iterator for an internal region
  region.SetSize( size );
  region.SetIndex( start );
  myIteratorType itb( inputImage, region );

  // Initialize the content the internal region
  while( !itb.IsAtEnd() )
    {
    itb.Set( myPixelFill );
    ++itb;
    }

  // Declare the type for the
  typedef itk::GradientRecursiveGaussianImageFilter<myImageType, myGradImageType >  myFilterType;
  typedef typename myFilterType::OutputImageType                                    myGradientImageType;

  // Create a  Filter
  typename myFilterType::Pointer filter = myFilterType::New();

  // Connect the input images
  filter->SetInput( inputImage );

  // Select the value of Sigma
  filter->SetSigma( 2.5 );

  // Execute the filter
  filter->Update();

  // Get the Smart Pointer to the Filter Output
  // It is important to do it AFTER the filter is Updated
  // Because the object connected to the output may be changed
  // by another during GenerateData() call
  outputImage = filter->GetOutput();

  // Write the output to file
  typedef itk::ImageFileWriter< myGradientImageType >   WriterType;

  typename WriterType::Pointer      writer =  WriterType::New();
  writer->SetFileName( outputFilename );
  writer->SetInput( outputImage );
  writer->Update();

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}

////////////////////////////////////////////////////////////////////

template< typename TGradImage1DType, typename TGradImageVectorType >
int itkGradientRecursiveGaussianFilterTest3Compare( typename TGradImage1DType::Pointer scalarPixelGradImage, typename TGradImageVectorType::Pointer vectorPixelGradImage,  unsigned int numDimensions )
{
  itk::ImageRegionIteratorWithIndex< TGradImage1DType > scalarIt( scalarPixelGradImage, scalarPixelGradImage->GetBufferedRegion() );
  itk::ImageRegionIteratorWithIndex< TGradImageVectorType > vector2DIt( vectorPixelGradImage, vectorPixelGradImage->GetBufferedRegion() );
  scalarIt.GoToBegin();
  vector2DIt.GoToBegin();
  typename TGradImage1DType::PixelType::ValueType tolerance = 1e-5;

  while( ! scalarIt.IsAtEnd() && ! vector2DIt.IsAtEnd() )
    {
    typename TGradImage1DType::PixelType scalar = scalarIt.Value();
    typename TGradImageVectorType::PixelType vector = vector2DIt.Value();
    for( unsigned int d=0; d < numDimensions; d++ )
      {
      for( unsigned int c=0; c < vector.GetNumberOfComponents() / numDimensions; c++)
        {
      typename TGradImage1DType::PixelType::ValueType truth = scalar[d] / ( c + 1.0);
      typename TGradImage1DType::PixelType::ValueType test = vector[d + (c * numDimensions)];
      if( std::fabs(  truth - test ) > tolerance )
          {
          std::cerr << "One or more components of vector gradient image pixel are not as expected: " << std::endl
                    << "d, c, truth, test: " << d << " " << c << " " << truth << " " << test << std::endl
                    << "scalar pixel gradient: " << scalar << " vector pixel gradient: " << vector << std::endl;
          return EXIT_FAILURE;
          }
        }
      }
    ++scalarIt;
    ++vector2DIt;
    }
  return EXIT_SUCCESS;
}

////////////////////////////////////////////////////////////////////

int itkGradientRecursiveGaussianFilterTest3(int argc, char *argv[] )
{
  if( argc != 8 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " outputImageFile1 outputImageFile2 outputImageFile3 outputImageFile4 outputImageFile5 outputImageFile6 outputImageFile7" << std::endl;
    return EXIT_FAILURE;
    }

  int result = EXIT_SUCCESS;

  const unsigned int myDimension = 3;
  const unsigned int myComponents1D = 1;
  typedef itk::Vector<float,myDimension*myComponents1D>   myGrad1DType;
  typedef itk::Image<myGrad1DType, myDimension>           myGradImage1DType;

  // Test with Image of 1D Vector
  typedef itk::Vector<float, myComponents1D>          myVector1DType;
  typedef itk::Image<myVector1DType, myDimension>     myImageVector1DType;

  myGradImage1DType::Pointer vector1DGradImage = ITK_NULLPTR;
  myVector1DType vector1Dborder;
  myVector1DType vector1Dfill;
  vector1Dborder.Fill( 0.0 );
  vector1Dfill.Fill( 100.0 );
  int runResult = itkGradientRecursiveGaussianFilterTest3Run<myImageVector1DType, myGradImage1DType, myComponents1D>( vector1Dborder, vector1Dfill, vector1DGradImage, argv[1] );
  if( runResult == EXIT_FAILURE )
    {
    std::cerr << "Failed with Image<1D-Vector> type." << std::endl;
    result = runResult;
    }

  // Test with Image of *scalar* pixels to verify same results
  typedef float                                            myScalarPixelType;
  typedef itk::Image<myScalarPixelType, myDimension>       myImageScalarType;

  myGradImage1DType::Pointer scalarPixelGradImage = ITK_NULLPTR;
  myScalarPixelType pixelBorder;
  myScalarPixelType pixelFill;
  pixelBorder = itk::NumericTraits<myScalarPixelType>::ZeroValue();
  pixelFill = static_cast<myScalarPixelType>(100.0);
  runResult = itkGradientRecursiveGaussianFilterTest3Run<myImageScalarType, myGradImage1DType, myComponents1D>( pixelBorder, pixelFill, scalarPixelGradImage, argv[2] );
  if( runResult == EXIT_FAILURE )
    {
    std::cerr << "Failed with scalar pixel type." << std::endl;
    result = runResult;
    }

  // Test with Image of 2D Vector
  const unsigned int myComponents2D = 2;
  typedef itk::Vector<float,myDimension*myComponents2D> myGrad2DType;
  typedef itk::Image<myGrad2DType, myDimension>         myGradImage2DType;
  typedef itk::Vector<float, myComponents2D>            myVector2DType;
  typedef itk::Image<myVector2DType, myDimension>       myImage2DType;
  typedef itk::VariableLengthVector<float>              myVarVector2DType;
  typedef itk::Image<myVarVector2DType, myDimension>    myImageVar2DType;

  myGradImage2DType::Pointer    vector2DGradImage = ITK_NULLPTR;
  myVector2DType                vector2Dborder;
  myVector2DType                vector2Dfill;
  vector2Dborder.Fill( pixelBorder );
  vector2Dfill[0] = pixelFill;
  vector2Dfill[1] = pixelFill / 2.0;
  runResult = itkGradientRecursiveGaussianFilterTest3Run<myImage2DType, myGradImage2DType, myComponents2D>( vector2Dborder, vector2Dfill, vector2DGradImage, argv[3] );
  if( runResult == EXIT_FAILURE )
    {
    std::cerr << "Failed with 2D Vector type." << std::endl;
    result = runResult;
    }

  // Compare the scalar pixel result to 2D vector result
  int compareResult = itkGradientRecursiveGaussianFilterTest3Compare<myGradImage1DType, myGradImage2DType>( scalarPixelGradImage, vector2DGradImage, myDimension );
  if( compareResult == EXIT_FAILURE )
    {
    std::cerr << "Failed for 2D-vector comparison." << std::endl;
    return EXIT_FAILURE;
    }

  // Test with Image of 2D VariableLengthVector
  myGradImage2DType::Pointer    varVector2DGradImage = ITK_NULLPTR;
  myVarVector2DType             varVector2Dborder;
  myVarVector2DType             varVector2Dfill;
  varVector2Dborder.SetSize( myComponents2D );
  varVector2Dfill.SetSize( myComponents2D );
  varVector2Dborder.Fill( 0.0 );
  varVector2Dfill[0] = 100.0;
  varVector2Dfill[1] = 50.0;
  runResult = itkGradientRecursiveGaussianFilterTest3Run<myImageVar2DType, myGradImage2DType, myComponents2D>( varVector2Dborder, varVector2Dfill, varVector2DGradImage, argv[4] );
  if( runResult == EXIT_FAILURE )
    {
    std::cerr << "Failed with 2D VariableLengthVector type." << std::endl;
    result = runResult;
    }

  // Test with 2D VectorImage
  typedef itk::VectorImage<float, myDimension> myVecImageType;
  myGradImage2DType::Pointer    vectorImage2DGradImage = ITK_NULLPTR;
  runResult = itkGradientRecursiveGaussianFilterTest3Run<myVecImageType, myGradImage2DType, myComponents2D>( varVector2Dborder, varVector2Dfill, vectorImage2DGradImage, argv[5] );
  if( runResult == EXIT_FAILURE )
    {
    std::cerr << "Failed with 2D-vector VectorImage type." << std::endl;
    result = runResult;
    }

  // Test with Image of 3D Vector
  const unsigned int myComponents3D = 3;
  typedef itk::Vector<float,myDimension*myComponents3D> myGrad3DType;
  typedef itk::Image<myGrad3DType, myDimension>         myGradImage3DType;
  typedef itk::Vector<float, myComponents3D>            myVector3DType;
  typedef itk::Image<myVector3DType, myDimension>       myImage3DType;

  myGradImage3DType::Pointer    vector3DGradImage = ITK_NULLPTR;
  myVector3DType                vector3Dborder;
  myVector3DType                vector3Dfill;
  vector3Dborder.Fill( pixelBorder );
  vector3Dfill[0] = pixelFill;
  vector3Dfill[1] = pixelFill / 2.0;
  vector3Dfill[2] = pixelFill / 3.0;
  runResult = itkGradientRecursiveGaussianFilterTest3Run<myImage3DType, myGradImage3DType, myComponents3D>( vector3Dborder, vector3Dfill, vector3DGradImage, argv[6] );
  if( runResult == EXIT_FAILURE )
    {
    std::cerr << "Failed with 3D Vector type." << std::endl;
    result = runResult;
    }

  // Compare the scalar pixel result to 3D vector result
  compareResult = itkGradientRecursiveGaussianFilterTest3Compare<myGradImage1DType, myGradImage3DType>( scalarPixelGradImage, vector3DGradImage, myDimension );
  if( compareResult == EXIT_FAILURE )
    {
    std::cerr << "Failed for 3D-vector comparison." << std::endl;
    return EXIT_FAILURE;
    }

  // Test with 3D VectorImage
  typedef itk::VectorImage<float, myDimension>          myVecImageType;
  typedef itk::VariableLengthVector<float>              myVarVector3DType;
  myGradImage3DType::Pointer    vectorImage3DGradImage = ITK_NULLPTR;
  myVarVector3DType             varVector3Dborder;
  myVarVector3DType             varVector3Dfill;
  varVector3Dborder.SetSize( myComponents3D );
  varVector3Dborder.Fill( vector3Dborder[0] );
  varVector3Dfill.SetSize( myComponents3D );
  varVector3Dfill[0] = vector3Dfill[0];
  varVector3Dfill[1] = vector3Dfill[1];
  varVector3Dfill[2] = vector3Dfill[2];
  runResult = itkGradientRecursiveGaussianFilterTest3Run<myVecImageType, myGradImage3DType, myComponents3D>( varVector3Dborder, varVector3Dfill, vectorImage3DGradImage, argv[7] );
  if( runResult == EXIT_FAILURE )
    {
    std::cerr << "Failed with 3D-vector VectorImage type." << std::endl;
    result = runResult;
    }

  return result;
}
