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

#include "itkCastImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkSymmetricSecondRankTensor.h"
#include "itkSymmetricEigenAnalysisImageFilter.h"
#include "itkTestingMacros.h"


namespace itk {

template< typename TInputImage, typename TInternalImage, typename TOutputImage >
class SymmetricEigenAnalysisImageFilterHelper :
  public SymmetricEigenAnalysisImageFilter< TInputImage, TInternalImage >
{
public:
  typedef SymmetricEigenAnalysisImageFilterHelper
    Self;
  typedef SymmetricEigenAnalysisImageFilter< TInputImage, TInternalImage >
    Superclass;

  typedef SmartPointer<Self>                          Pointer;
  typedef SmartPointer<const Self>                    ConstPointer;


  typedef TInputImage                                 InputImageType;
  typedef TInternalImage                              InternalImageType;
  typedef TOutputImage                                OutputImageType;


  itkTypeMacro( SymmetricEigenAnalysisImageFilterHelper,
    SymmetricEigenAnalysisImageFilter );

  itkNewMacro( Self );

  static int Exercise( typename Superclass::FunctorType::EigenValueOrderType order,
    std::string outputFilename )
  {

    typedef SymmetricEigenAnalysisImageFilter<
      InputImageType, InternalImageType > SymmetricEigenAnalysisImageFilterType;

    // Declare the type of the index to access images
    typedef itk::Index< InputImageType::ImageDimension >        IndexType;

    // Declare the type of the size
    typedef itk::Size< InputImageType::ImageDimension >         SizeType;

    // Declare the type of the Region
    typedef itk::ImageRegion< InputImageType::ImageDimension >  RegionType;

    // Create the input image
    typename InputImageType::Pointer inputImage = InputImageType::New();

    // Define its size, and start index
    SizeType size;
    size[0] = 8;
    size[1] = 8;
    size[2] = 8;

    IndexType start;
    start.Fill(0);

    RegionType region;
    region.SetIndex( start );
    region.SetSize( size );

    // Initialize the input image
    inputImage->SetLargestPossibleRegion( region );
    inputImage->SetBufferedRegion( region );
    inputImage->SetRequestedRegion( region );
    inputImage->Allocate();

    // Declare Iterator type for the input image
    typedef itk::ImageRegionIteratorWithIndex< InputImageType >  IteratorType;

    // Create one iterator for the input image (this is a light object)
    IteratorType it( inputImage, inputImage->GetRequestedRegion() );

    typename InputImageType::PixelType tensorValue;

    tensorValue(0,0) = 19.0;
    tensorValue(0,1) = 23.0;
    tensorValue(0,2) = 29.0;
    tensorValue(1,1) = 31.0;
    tensorValue(1,2) = 37.0;
    tensorValue(2,2) = 39.0;

    it.GoToBegin();

    // Initialize the content of the input image
    while( !it.IsAtEnd() )
      {
      it.Set( tensorValue );
      ++it;
      }


    // Create the filter
    typename SymmetricEigenAnalysisImageFilterType::Pointer filter =
      SymmetricEigenAnalysisImageFilterType::New();

    filter->SetDimension( InputImageType::ImageDimension );
    TEST_SET_GET_VALUE( InputImageType::ImageDimension, filter->GetDimension() );

    // Set the input image
    filter->SetInput( inputImage );

    filter->SetFunctor( filter->GetFunctor() );

    filter->OrderEigenValuesBy( order );

    // Execute the filter
    TRY_EXPECT_NO_EXCEPTION( filter->Update() );

    // Get the filter output
    // It is important to do it AFTER the filter is Updated
    // Because the object connected to the output may be changed
    // by another during GenerateData() call
    typename InternalImageType::Pointer internalImage = filter->GetOutput();

    // Get the output image to a writable format
    typedef itk::CastImageFilter< InternalImageType, OutputImageType >
      CastImageFilterType;

    typename CastImageFilterType::Pointer roundImageFilter =
      CastImageFilterType::New();

    roundImageFilter->SetInput( internalImage );

    TRY_EXPECT_NO_EXCEPTION( roundImageFilter->Update() );

    // Write the result image
    typedef itk::ImageFileWriter< OutputImageType > WriterType;

    typename WriterType::Pointer writer = WriterType::New();

    writer->SetFileName( outputFilename );

    writer->SetInput( roundImageFilter->GetOutput() );

    TRY_EXPECT_NO_EXCEPTION( writer->Update() );

    std::cout << "Test succeeded." << std::endl;
    return EXIT_SUCCESS;
  }

};

} // end namespace itk


int itkSymmetricEigenAnalysisImageFilterTest( int argc, char* argv[] )
{

  if ( argc < 3 )
  {
  std::cout << "Usage: " << argv[0]
    << "outputImage order " << std::endl;
  return EXIT_FAILURE;
  }

  // Define the dimension of the images
  const unsigned int Dimension = 3;

  // Declare the pixel type
  typedef float                 InputPixelType;
  typedef double                InternalPixelType;
  typedef unsigned char         OutputPixelType;

  // Define the symmetric tensor pixel type
  typedef itk::SymmetricSecondRankTensor< InputPixelType, Dimension > TensorType;

  // Declare the types of the images
  typedef itk::Image< TensorType, Dimension >               InputImageType;

  // Define the type for storing the eigen-value
  typedef itk::FixedArray< InternalPixelType, Dimension >   InternalValueArray;
  typedef itk::FixedArray< OutputPixelType, Dimension >     OutputValueArray;

  // Declare the types of the output images
  typedef itk::Image< InternalValueArray, Dimension >       InternalImageType;
  typedef itk::Image< OutputValueArray, Dimension >         OutputImageType;

  // Declare the type for the filter
  typedef itk::SymmetricEigenAnalysisImageFilter<
                                     InputImageType,
                                     InternalImageType > FilterType;

  // Create an instance to exercise basic object methods
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, SymmetricEigenAnalysisImageFilter,
    UnaryFunctorImageFilter );


  // Get the input arguments
  FilterType::FunctorType::EigenValueOrderType order =
    static_cast< FilterType::FunctorType::EigenValueOrderType >( atoi( argv[2] ) );

  std::string outputFilename = argv[1];


  // Test the filter
  int testResult = itk::SymmetricEigenAnalysisImageFilterHelper< InputImageType,
    InternalImageType, OutputImageType >::Exercise( order, outputFilename );


  // All objects should be automatically destroyed at this point
  return testResult;
}
