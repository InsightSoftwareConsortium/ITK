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

// Software Guide : BeginLatex
//
// \index{itk::TransformReader}
// \index{itk::TransformWriter}
//
// This example shows how to read and write a transform
// using the \doxygen{TransformFileReader} and
// \doxygen{TransformFileWriter}.
// Let's first include the two appropriate header files.
//
// Software Guide : EndLatex
// Software Guide : BeginCodeSnippet
#include "itkTransformFileReader.h"
#include "itkTransformFileWriter.h"
// Software Guide : EndCodeSnippet

#include "itkAffineTransform.h"
#include "itkBSplineTransform.h"
#include "itkCompositeTransform.h"
#include "itkTransformFactory.h"

int main( int argc, char * argv[] )
{
  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " transformFile" << std::endl;
    return EXIT_FAILURE;
    }
  const char * transformFileName = argv[1];

  typedef double ScalarType;
  const unsigned int Dimension = 3;

  typedef itk::CompositeTransform< ScalarType, Dimension > CompositeTransformType;
  CompositeTransformType::Pointer composite = CompositeTransformType::New();

  typedef itk::AffineTransform< ScalarType, Dimension > AffineTransformType;
  AffineTransformType::Pointer affine = AffineTransformType::New();
  AffineTransformType::InputPointType cor;
  cor.Fill(12);
  affine->SetCenter(cor);

  composite->AddTransform( affine );

  const unsigned int SplineOrder = 5;
  typedef itk::BSplineTransform< ScalarType, Dimension, SplineOrder >
    BSplineTransformType;
  typedef itk::BSplineTransform< float, Dimension, SplineOrder >
    BSplineTransformFType;

  // By default only BSpline transforms of order 3 are registered.
  // Manually register this order 5 bspline for both float and double
  // scalar types, so that we can read the correct order transform.
  itk::TransformFactory<BSplineTransformType>::RegisterTransform();
  itk::TransformFactory<BSplineTransformFType>::RegisterTransform();

  BSplineTransformType::Pointer bspline = BSplineTransformType::New();

  BSplineTransformType::OriginType origin;
  origin.Fill( 100 );
  BSplineTransformType::PhysicalDimensionsType dimensions;
  dimensions.Fill( 1.5 * 9.0 );

  bspline->SetTransformDomainOrigin( origin );
  bspline->SetTransformDomainPhysicalDimensions( dimensions );

  BSplineTransformType::ParametersType parameters( bspline->GetNumberOfParameters() );
  bspline->SetParameters( parameters );
  bspline->SetIdentity();

  composite->AddTransform( bspline );

  // Software Guide : BeginLatex
  //
  // The transform reader and writer is templated. The conversion is precision
  // done if necessary when writing or reading the file. We create a writer
  // using smart pointers.
  //
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef itk::TransformFileWriterTemplate< ScalarType > TransformWriterType;
  TransformWriterType::Pointer writer = TransformWriterType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We add a CompositeTransform with the
  // SetInput() function. This function takes any \doxygen{Transform}
  //
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  writer->SetInput( composite );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Then we set the filename using the SetFileName() function.
  // Then we call the Update()function to write the transform(s)
  // onto the disk.
  //
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  writer->SetFileName( transformFileName );
  // Software Guide : EndCodeSnippet
  try
    {
    // Software Guide : BeginCodeSnippet
    writer->Update();
    // Software Guide : EndCodeSnippet
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while saving the transforms" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  //
  // In order to read a transform file, we instantiate a TransformFileReader.
  // Like the writer, the reader is templated.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef float ReadScalarType;

  typedef itk::TransformFileReaderTemplate< ReadScalarType >
    TransformReaderType;
  TransformReaderType::Pointer reader = TransformReaderType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We then set the name of the file we want to read, and call the
  // Update() function.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  reader->SetFileName( transformFileName );
  // Software Guide : EndCodeSnippet

  try
    {
  // Software Guide : BeginCodeSnippet
    reader->Update();
  // Software Guide : EndCodeSnippet
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while reading the transform file" << std::endl;
    std::cerr << excp << std::endl;
    std::cerr << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  //
  // The transform reader is templated and it returns a list
  // of \doxygen{Transform}'s. Even thought the reader instantiate the appropriate
  // transform class when reading the file, it is up to the user to
  // do the approriate cast.
  // To get the output list of transform we use the GetTransformList() function.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const TransformReaderType::TransformListType * transforms =
    reader->GetTransformList();
  std::cout << "Number of transforms = " << transforms->size() << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We then use an STL iterator to go through the list of transforms. We show here
  // how to do the proper casting of the resulting transform.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::CompositeTransform< ReadScalarType, Dimension >
    ReadCompositeTransformType;
  TransformReaderType::TransformListType::const_iterator it
    = transforms->begin();
  if( !strcmp((*it)->GetNameOfClass(),"CompositeTransform") )
    {
    ReadCompositeTransformType::Pointer compositeRead
      = static_cast< ReadCompositeTransformType* >( (*it).GetPointer() );
    compositeRead->Print(std::cout);
    }
  //  Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
