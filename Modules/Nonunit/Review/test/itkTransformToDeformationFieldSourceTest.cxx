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
#include "itkAffineTransform.h"
#include "itkBSplineDeformableTransform.h"
#include "itkTransformToDeformationFieldSource.h"
#include "itkImageFileWriter.h"

int itkTransformToDeformationFieldSourceTest( int argc, char * argv [] )
{
  /** Check command line arguments. */
  if( argc < 3 )
    {
    std::cerr << "You must supply transform and output filename" << std::endl;
    return EXIT_FAILURE;
    }

  std::string transformName = argv[ 1 ];
  std::string fileName = argv[ 2 ];
  std::string bSplineParametersFile;
  if ( argc > 3 )
    {
    bSplineParametersFile = argv[ 3 ];
    }

  /** Typedefs. */
  const unsigned int  Dimension = 2;
  typedef float       ScalarPixelType;
  typedef double      CoordRepresentationType;
  const unsigned int  SplineOrder = 3;

  typedef itk::Vector<
    ScalarPixelType, Dimension >          VectorPixelType;

  typedef itk::Image<
    VectorPixelType, Dimension >          DeformationFieldImageType;

  typedef itk::Transform<
    CoordRepresentationType, Dimension,
    Dimension >                           TransformType;

  typedef itk::AffineTransform<
    CoordRepresentationType, Dimension >  AffineTransformType;

  typedef itk::BSplineDeformableTransform<
    CoordRepresentationType, Dimension,
    SplineOrder >                         BSplineDeformableTransformType;

  typedef TransformType::ParametersType   ParametersType;

  typedef itk::TransformToDeformationFieldSource<
    DeformationFieldImageType,
    CoordRepresentationType >             DeformationFieldGeneratorType;

  typedef DeformationFieldGeneratorType::SizeType       SizeType;
  typedef DeformationFieldGeneratorType::SpacingType    SpacingType;
  typedef DeformationFieldGeneratorType::OriginType     OriginType;
  typedef DeformationFieldGeneratorType::IndexType      IndexType;
  typedef DeformationFieldGeneratorType::RegionType     RegionType;
  typedef itk::ImageFileWriter<
    DeformationFieldImageType >                         WriterType;

  /** Create output information. */
  SizeType size;        size.Fill( 20 );
  IndexType index;      index.Fill( 0 );
  SpacingType spacing;  spacing.Fill( 0.7 );
  OriginType origin;    origin.Fill( -10.0 );

  /** Create transforms. */
  AffineTransformType::Pointer affineTransform
    = AffineTransformType::New();
  BSplineDeformableTransformType::Pointer bSplineTransform
    = BSplineDeformableTransformType::New();
  if ( transformName == "Affine" )
    {
    /** Set the options. */
    OriginType centerOfRotation;
    centerOfRotation[ 0 ] = -3.0; centerOfRotation[ 1 ] = -3.0;
    affineTransform->SetCenter( centerOfRotation );

    /** Create and set parameters. */
    ParametersType parameters( affineTransform->GetNumberOfParameters() );
    parameters[ 0 ] =   1.1;
    parameters[ 1 ] =   0.1;
    parameters[ 2 ] =  -0.2;
    parameters[ 3 ] =   0.9;
    parameters[ 4 ] =  10.3;
    parameters[ 5 ] = -33.8;
    affineTransform->SetParameters( parameters );
    }
  else if ( transformName == "BSpline" )
    {
    /** Set the options. */
    SizeType gridSize;
    gridSize[ 0 ] = 7; gridSize[ 1 ] = 10;
    //gridSize[ 0 ] = 2; gridSize[ 1 ] = 3;
    IndexType gridIndex; gridIndex.Fill( 0 );
    RegionType gridRegion;
    gridRegion.SetSize( gridSize );
    gridRegion.SetIndex( gridIndex );
    SpacingType gridSpacing;
    gridSpacing[ 0 ] = 5.4; gridSpacing[ 1 ] = 3.1;
    OriginType gridOrigin; gridOrigin.Fill( -16.0 );
    bSplineTransform->SetGridOrigin( gridOrigin );
    bSplineTransform->SetGridSpacing( gridSpacing );
    bSplineTransform->SetGridRegion( gridRegion );

    /** Create and set parameters. */
    ParametersType parameters( bSplineTransform->GetNumberOfParameters() );
    std::ifstream input( bSplineParametersFile.c_str() );
    if ( input.is_open() )
      {
      for ( unsigned int i = 0; i < parameters.GetSize(); ++i )
        {
        input >> parameters[ i ];
        }
      input.close();
      }
    else
      {
      std::cerr << "ERROR: B-spline parameter file not found." << std::endl;
      return EXIT_FAILURE;
      }
    bSplineTransform->SetParametersByValue( parameters );
    }
  else
    {
    std::cerr << "ERROR: Not a valid transform." << std::endl;
    return EXIT_FAILURE;
    }

  /** Create an setup deformation field generator. */
  DeformationFieldGeneratorType::Pointer defGenerator
    = DeformationFieldGeneratorType::New();
  std::cout << "Name of Class: " << defGenerator->GetNameOfClass()
            << std::endl;
  defGenerator->SetOutputSize( size );
  defGenerator->SetOutputSpacing( spacing );
  defGenerator->SetOutputOrigin( origin );
  defGenerator->SetOutputIndex( index );
  //
  // for coverage, exercise access methods
  spacing  = defGenerator->GetOutputSpacing();
  origin = defGenerator->GetOutputOrigin();
  DeformationFieldGeneratorType::DirectionType
    direction = defGenerator->GetOutputDirection();
  std::cout << "Spacing " << spacing
            << " Origin " << origin
            << std::endl << "Direction "
            << direction
            << std::endl;
  //defGenerator->SetOutputDirection( direction );
  if ( transformName == "Affine" )
    {
    defGenerator->SetTransform( affineTransform );
    }
  else if ( transformName == "BSpline" )
    {
    defGenerator->SetTransform( bSplineTransform );
    }
  std::cout << "Transform: " << defGenerator->GetTransform()
            << std::endl;
  DeformationFieldGeneratorType::OutputImageRegionType
    outputRegion = defGenerator->GetOutputRegion();
  defGenerator->SetOutputRegion(outputRegion);

  /** Write deformation field to disk. */
  WriterType::Pointer writer
    = WriterType::New();
  writer->SetInput( defGenerator->GetOutput() );
  writer->SetFileName( fileName.c_str() );

  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject & e )
    {
    std::cerr << "Exception detected while generating deformation field" << argv[1];
    std::cerr << " : "  << e.GetDescription();
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

} // end main
