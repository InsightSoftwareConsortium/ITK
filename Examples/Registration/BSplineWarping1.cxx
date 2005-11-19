/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    BSplineWarping1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif


//  Software Guide : BeginLatex
//
//  This example illustrates how to deform an image using a BSplineTransform.
// 
//  \index{BSplineDeformableTransform}
//
//  Software Guide : EndLatex 



// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h" 
#include "itkImageFileWriter.h" 

#include "itkImage.h"
#include "itkResampleImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"

#include "itkBSplineDeformableTransform.h"

#include <fstream>


int main( int argc, char * argv[] )
{

  if( argc < 5 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " coefficientsFile fixedImage ";
    std::cerr << "movingImage deformedMovingImage" << std::endl;
    std::cerr << "[deformationField]" << std::endl;
    return 1;
    }

  const     unsigned int   ImageDimension = 2;

  typedef   unsigned char  PixelType;
  typedef   itk::Image< PixelType, ImageDimension >  FixedImageType;
  typedef   itk::Image< PixelType, ImageDimension >  MovingImageType;

  typedef   itk::ImageFileReader< FixedImageType  >  FixedReaderType;
  typedef   itk::ImageFileReader< MovingImageType >  MovingReaderType;

  typedef   itk::ImageFileWriter< MovingImageType >  MovingWriterType;


  FixedReaderType::Pointer fixedReader = FixedReaderType::New();
  fixedReader->SetFileName( argv[2] );

  try
    {
    fixedReader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  MovingReaderType::Pointer movingReader = MovingReaderType::New();
  MovingWriterType::Pointer movingWriter = MovingWriterType::New();

  movingReader->SetFileName( argv[3] );
  movingWriter->SetFileName( argv[4] );


  FixedImageType::ConstPointer fixedImage = fixedReader->GetOutput();


  typedef itk::ResampleImageFilter< MovingImageType, 
                                    FixedImageType  >  FilterType;

  FilterType::Pointer resampler = FilterType::New();

  typedef itk::LinearInterpolateImageFunction< 
                       MovingImageType, double >  InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  resampler->SetInterpolator( interpolator );

  FixedImageType::SpacingType fixedSpacing = fixedImage->GetSpacing();
  FixedImageType::PointType   fixedOrigin  = fixedImage->GetOrigin();

  resampler->SetOutputSpacing( fixedSpacing );
  resampler->SetOutputOrigin(  fixedOrigin  );

  
  FixedImageType::RegionType fixedRegion = fixedImage->GetBufferedRegion();
  FixedImageType::SizeType   fixedSize =  fixedRegion.GetSize();
  resampler->SetSize( fixedSize );
  resampler->SetOutputStartIndex(  fixedRegion.GetIndex() );


  resampler->SetInput( movingReader->GetOutput() );
  
  movingWriter->SetInput( resampler->GetOutput() );
//  Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  We instantiate now the type of the \code{BSplineDeformableTransform} using
//  as template parameters the type for coordinates representation, the
//  dimension of the space, and the order of the B-spline. 
// 
//  \index{BSplineDeformableTransform!New}
//  \index{BSplineDeformableTransform!Instantiation}
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet

  const unsigned int SpaceDimension = ImageDimension;
  const unsigned int SplineOrder = 3;
  typedef double CoordinateRepType;

  typedef itk::BSplineDeformableTransform<
                            CoordinateRepType,
                            SpaceDimension,
                            SplineOrder >     TransformType;
  
  TransformType::Pointer bsplineTransform = TransformType::New();

//  Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  Since we are using a B-spline of order 3, the coverage of the BSpling grid
//  should exceed by one the spatial extent of the image on the lower region of
//  image indices, and by two grid points on the upper region of image indices.
//  We choose here to use a $8 \times 8$ B-spline grid, from which only a $5
//  \times 5$ sub-grid will be covering the input image. If we use an input
//  image of size $500 \times 500$ pixels, and pixel spacing $2.0 \times 2.0$
//  then we need the $5 \times 5$ B-spline grid to cover a physical extent of $1000
//  \times 1000$ mm. This can be achieved by setting the pixel spacing of the
//  B-spline grid to $250.0 \times 250.0$ mm. The origin of the B-spline grid
//  must be set at one grid position away from the origin of the desired output
//  image. All this is done with the following lines of code.
// 
//  \index{BSplineDeformableTransform}
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  typedef TransformType::RegionType RegionType;
  RegionType bsplineRegion;
  RegionType::SizeType   size;

  const unsigned int numberOfGridNodesOutsideTheImageSupport = 3;

  const unsigned int numberOfGridNodesInsideTheImageSupport = 5;

  const unsigned int numberOfGridNodes = 
                        numberOfGridNodesInsideTheImageSupport +
                        numberOfGridNodesOutsideTheImageSupport;

  const unsigned int numberOfGridCells = 
                        numberOfGridNodesInsideTheImageSupport - 1;
                        
  size.Fill( numberOfGridNodes );
  bsplineRegion.SetSize( size );

  typedef TransformType::SpacingType SpacingType;
  SpacingType spacing;
  spacing[0] = floor( fixedSpacing[0] * (fixedSize[0] - 1) / numberOfGridCells );
  spacing[1] = floor( fixedSpacing[1] * (fixedSize[1] - 1) / numberOfGridCells );

  typedef TransformType::OriginType OriginType;
  OriginType origin;
  origin[0] = fixedOrigin[0] - spacing[0];
  origin[1] = fixedOrigin[1] - spacing[1];
  
  bsplineTransform->SetGridSpacing( spacing );
  bsplineTransform->SetGridOrigin( origin );
  bsplineTransform->SetGridRegion( bsplineRegion );
  

  typedef TransformType::ParametersType     ParametersType;

  const unsigned int numberOfParameters =
               bsplineTransform->GetNumberOfParameters();
  

  const unsigned int numberOfNodes = numberOfParameters / SpaceDimension;

  ParametersType parameters( numberOfParameters );
//  Software Guide : EndCodeSnippet




//  Software Guide : BeginLatex
//
//  The B-spline grid should now be fed with coeficients at each node. Since
//  this is a two dimensional grid, each node should receive two coefficients.
//  Each coefficient pair is representing a displacement vector at this node.
//  The coefficients can be passed to the B-spline in the form of an array where
//  the first set of elements are the first component of the displacements for
//  all the nodes, and the second set of elemets is formed by the second
//  component of the displacements for all the nodes.
//
//  In this example we read such displacements from a file, but for convinience
//  we have written this file using the pairs of $(x,y)$ displacement for every
//  node. The elements read from the file should therefore be reorganized when
//  assigned to the elements of the array. We do this by storing all the odd
//  elements from the file in the first block of the array, and all the even
//  elements from the file in the second block of the array. Finally the array
//  is passed to the B-spline transform using the \code{SetParameters()}.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  std::ifstream infile;

  infile.open( argv[1] );

  for( unsigned int n=0; n < numberOfNodes; n++ )
    {
    infile >>  parameters[n]; 
    infile >>  parameters[n+numberOfNodes]; 
    } 

  infile.close();
//  Software Guide : EndCodeSnippet



//  Software Guide : BeginLatex
//
//   Finally the array is passed to the B-spline transform using the
//   \code{SetParameters()}.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet

  bsplineTransform->SetParameters( parameters );

//  Software Guide : EndCodeSnippet



  

//  Software Guide : BeginLatex
//
//  At this point we are ready to use the transform as part of the resample
//  filter. We trigger the execution of the pipeline by invoking
//  \code{Update()} on the last filter of the pipeline, in this case writer.
//  
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  resampler->SetTransform( bsplineTransform );
  
  try
    {
    movingWriter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }
//  Software Guide : EndCodeSnippet


  typedef itk::Point<  float, ImageDimension >  PointType;
  typedef itk::Vector< float, ImageDimension >  VectorType;
  typedef itk::Image< VectorType, ImageDimension >  DeformationFieldType;

  DeformationFieldType::Pointer field = DeformationFieldType::New();
  field->SetRegions( fixedRegion );
  field->SetOrigin( fixedOrigin );
  field->SetSpacing( fixedSpacing );
  field->Allocate();

  typedef itk::ImageRegionIterator< DeformationFieldType > FieldIterator;
  FieldIterator fi( field, fixedRegion );

  fi.GoToBegin();

  TransformType::InputPointType  fixedPoint;
  TransformType::OutputPointType movingPoint;
  DeformationFieldType::IndexType index;

  VectorType displacement;

  while( ! fi.IsAtEnd() )
    {
    index = fi.GetIndex();
    field->TransformIndexToPhysicalPoint( index, fixedPoint );
    movingPoint = bsplineTransform->TransformPoint( fixedPoint );
    displacement[0] = movingPoint[0] - fixedPoint[0];
    displacement[1] = movingPoint[1] - fixedPoint[1];
    fi.Set( displacement );
    ++fi;
    }



  typedef itk::ImageFileWriter< DeformationFieldType >  FieldWriterType;
  FieldWriterType::Pointer fieldWriter = FieldWriterType::New();

  fieldWriter->SetInput( field );

  if( argc >= 6 )
    {
    fieldWriter->SetFileName( argv[5] );
    try
      {
      fieldWriter->Update();
      }
    catch( itk::ExceptionObject & excp )
      {
      std::cerr << "Exception thrown " << std::endl;
      std::cerr << excp << std::endl;
      return EXIT_FAILURE;
      }
    }




  return EXIT_SUCCESS;
}

