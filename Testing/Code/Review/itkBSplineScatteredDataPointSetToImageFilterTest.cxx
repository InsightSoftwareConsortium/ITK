/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineScatteredDataPointSetToImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkPointSet.h"
#include "itkBSplineScatteredDataPointSetToImageFilter.h"

/**
 * In this test, we approximate a 2-D scalar field.
 * The scattered data is derived from a segmented 
 * image.  We write the output to an image for
 * comparison.
 */
int itkBSplineScatteredDataPointSetToImageFilterInternalTest1( int argc, char **argv )
{
  const unsigned int ParametricDimension = 2;
  const unsigned int DataDimension = 1;

  typedef int PixelType;
  typedef itk::Image<PixelType, ParametricDimension> InputImageType;
  typedef float RealType;
  typedef itk::Vector<RealType, DataDimension> VectorType;
  typedef itk::Image<VectorType, ParametricDimension> VectorImageType;
  typedef itk::PointSet
    <VectorImageType::PixelType, ParametricDimension> PointSetType;
  PointSetType::Pointer pointSet = PointSetType::New();  

  typedef itk::ImageFileReader<InputImageType> ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->Update();

  itk::ImageRegionIteratorWithIndex<InputImageType> 
    It( reader->GetOutput(), reader->GetOutput()->GetLargestPossibleRegion() );
  
  // Iterate through the input image which consists of multivalued 
  // foreground pixels (=nonzero) and background values (=zero).
  // The foreground pixels comprise the input point set.
  
  for ( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    if ( It.Get() != itk::NumericTraits<PixelType>::Zero )
      {
      // We extract both the 2-D location of the point 
      // and the pixel value of that point.  

      PointSetType::PointType point;
      reader->GetOutput()->TransformIndexToPhysicalPoint( It.GetIndex(), point );

      unsigned long i = pointSet->GetNumberOfPoints();
      pointSet->SetPoint( i, point );        

      PointSetType::PixelType V( DataDimension );
      V[0] = static_cast<RealType>( It.Get() );
      pointSet->SetPointData( i, V );
      }
    }

  
  // Instantiate the B-spline filter and set the desired parameters.
  typedef itk::BSplineScatteredDataPointSetToImageFilter
    <PointSetType, VectorImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetSplineOrder( 3 );  
  FilterType::ArrayType ncps;  
  ncps.Fill( 4 );  
  filter->SetNumberOfControlPoints( ncps );
  filter->SetNumberOfLevels( 3 );

  // Define the parametric domain.
  filter->SetOrigin( reader->GetOutput()->GetOrigin() );
  filter->SetSpacing( reader->GetOutput()->GetSpacing() );
  filter->SetSize( reader->GetOutput()->GetLargestPossibleRegion().GetSize() );

  filter->SetInput( pointSet );

  try 
    {
    filter->Update();
    }
  catch (...) 
    {
    std::cerr << "Test 1: itkBSplineScatteredDataImageFilter exception thrown" 
              << std::endl;
    return EXIT_FAILURE;
    }
  
  // Write the output to an image.
  typedef itk::Image<RealType, ParametricDimension> RealImageType;
  RealImageType::Pointer image = RealImageType::New();
  image->SetRegions( reader->GetOutput()->GetLargestPossibleRegion() );
  image->Allocate();
  itk::ImageRegionIteratorWithIndex<RealImageType> 
    Itt( image, image->GetLargestPossibleRegion() );
  
  for ( Itt.GoToBegin(); !Itt.IsAtEnd(); ++Itt )
    {
    Itt.Set( filter->GetOutput()->GetPixel( Itt.GetIndex() )[0] );
    }

  typedef itk::ImageFileWriter<RealImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( image );
  writer->SetFileName( argv[2] );
  writer->Update();

  return EXIT_SUCCESS; 
};

/**
 * In this example, we sample a parametric curve (helix)
 * and reconstruct using B-splines.
 */
int itkBSplineScatteredDataPointSetToImageFilterInternalTest2()
{
  const unsigned int ParametricDimension = 1;
  const unsigned int DataDimension = 3;

  typedef double RealType;
  typedef itk::Vector<RealType, DataDimension> VectorType;
  typedef itk::Image<VectorType, ParametricDimension> ImageType;  

  typedef itk::PointSet<VectorType, ParametricDimension> PointSetType;
  PointSetType::Pointer pointSet = PointSetType::New();  

  // Sample the helix.
  for ( RealType t = 0.0; t <= 1.0+1e-10; t += 0.05 )
    {
    unsigned long i = pointSet->GetNumberOfPoints();

    PointSetType::PointType point;
    point[0] = t;
    pointSet->SetPoint( i, point );        

    VectorType V;
    V[0] = 0.25*cos(t*6.0*3.141);
    V[1] = 0.25*sin(t*6.0*3.141);
    V[2] = 4.0*t;

    pointSet->SetPointData( i, V );
    }

  // Instantiate the filter and set the parameters
  typedef itk::BSplineScatteredDataPointSetToImageFilter
     <PointSetType, ImageType>  FilterType;
  FilterType::Pointer filter = FilterType::New();
  
  // Define the parametric domain
  ImageType::SpacingType spacing;  
  spacing.Fill( 0.001 );
  ImageType::SizeType size;  
  size.Fill( static_cast<unsigned int>( 1.0/spacing[0] ) + 1 );
  ImageType::PointType origin;  
  origin.Fill( 0.0 );

  filter->SetSize( size );
  filter->SetOrigin( origin );
  filter->SetSpacing( spacing );
  filter->SetInput( pointSet );

  filter->SetSplineOrder( 3 );  
  FilterType::ArrayType ncps;
  ncps.Fill( 4 );  
  filter->SetNumberOfControlPoints( ncps );
  filter->SetNumberOfLevels( 5 );
  filter->SetGenerateOutputImage( false );

  try 
    {
    filter->Update();
    
    for ( RealType t = 0.0; t <= 1.0+1e-10; t += 0.01 )
      {
      PointSetType::PointType point;
      point[0] = t;

      VectorType V; 
      filter->Evaluate( point, V );

      FilterType::GradientType G;
      filter->EvaluateGradient( point, G );

      }
    }
  catch (...) 
    {
    std::cerr << "Test 2: itkBSplineScatteredDataImageFilter exception thrown" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
};

int itkBSplineScatteredDataPointSetToImageFilterTest( int argc, char **argv )
{
  if ( argc != 3 )
    {
    std::cout << "Usage: " << argv[0] << " inputImage outputImage" << std::endl;
    return EXIT_FAILURE;
    }
  
  bool test1 = itkBSplineScatteredDataPointSetToImageFilterInternalTest1( argc, argv );
  bool test2 = itkBSplineScatteredDataPointSetToImageFilterInternalTest2();
  
  return ( test1 && test2 );
}














