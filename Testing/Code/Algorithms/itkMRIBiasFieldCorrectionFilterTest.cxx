/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMRIBiasFieldCorrectionFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <iostream>
#include "vnl/vnl_vector.h"

#include "itkMRIBiasFieldCorrectionFilter.h"
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkGaussianImageSource.h"
#include "itkMultivariateLegendrePolynomial.h"
#include "itkCompositeValleyFunction.h"
#include "itkNormalVariateGenerator.h"
#include "itkArray.h"
#include "itkImageFileWriter.h"
#include "itkSphereSpatialFunction.h"

int itkMRIBiasFieldCorrectionFilterTest ( int , char* [] )
{
  typedef itk::Image< float, 3 > ImageType ;
  typedef itk::Image< unsigned char, 3 > MaskType ;
  typedef itk::ImageRegionIteratorWithIndex< ImageType > ImageIteratorType ;

  bool SaveImages = false ;
  ImageType::SizeType imageSize ;
  ImageType::IndexType imageIndex ;
  ImageType::RegionType imageRegion ;
  imageSize[0] = 30 ;
  imageSize[1] = 30 ;
  imageSize[2] = 10 ;
  imageIndex.Fill( 0 ) ;
  float spacing[3] = {1.0, 1.0, 1.0} ;
  float origin[3] = {0, 0, 0} ;

  imageRegion.SetSize( imageSize ) ;
  imageRegion.SetIndex( imageIndex ) ;

  // creates an image that will stores the Gaussian pixel * bias values
  ImageType::Pointer imageWithBias = ImageType::New() ;
  imageWithBias->SetBufferedRegion( imageRegion ) ;
  imageWithBias->SetLargestPossibleRegion( imageRegion ) ;
  imageWithBias->SetSpacing( spacing ) ;
  imageWithBias->SetOrigin( origin ) ;
  imageWithBias->Allocate() ;

  // creates the image source with a sphere.
  ImageType::Pointer image = ImageType::New() ;
  image->SetBufferedRegion( imageRegion ) ;
  image->SetLargestPossibleRegion( imageRegion ) ;
  image->SetSpacing( spacing ) ;
  image->SetOrigin( origin ) ;
  image->Allocate() ;

  // creates an image for bias
  ImageType::Pointer biasImage = ImageType::New() ;
  biasImage->SetBufferedRegion( imageRegion ) ;
  biasImage->SetLargestPossibleRegion( imageRegion ) ;
  biasImage->SetSpacing( spacing ) ;
  biasImage->SetOrigin( origin ) ;
  biasImage->Allocate() ;

  // class statistics for two classes: a bright sphere and background 
  itk::Array<double> classMeans(2) ;
  itk::Array<double> classSigmas(2) ;

  classMeans[0] = 10.0 ;
  classMeans[1] = 200.0 ;

  classSigmas[0] = 1.0 ;
  classSigmas[1] = 10.0 ;

  // creats a normal random variate generator
  itk::Statistics::NormalVariateGenerator::Pointer randomGenerator =
    itk::Statistics::NormalVariateGenerator::New() ;

  // fills the image with a sphere filled with intensity values from a
  // normal distribution. 
  typedef itk::SphereSpatialFunction<3> SphereType;
  SphereType::Pointer sphere = SphereType::New();

  SphereType::InputType center;
  center[0] = imageSize[0]/2;
  center[1] = imageSize[1]/2;
  center[2] = imageSize[2]/2;
  sphere->SetCenter( center );
  sphere->SetRadius( 5.0 ) ;

  randomGenerator->Initialize( 2003 ) ;
  ImageIteratorType i_iter( image , imageRegion ) ;
  SphereType::InputType point ;
  while ( !i_iter.IsAtEnd() )
    {
    image->TransformIndexToPhysicalPoint( i_iter.GetIndex() , point ) ;
    if ( sphere->Evaluate( point ) == 1 ) // inside or on surface
      {
      i_iter.Set( randomGenerator->GetVariate() * classSigmas[1] 
                  + classMeans[1] ) ;
      }
    else
      {
      i_iter.Set( classMeans[0] ) ;
      }
    ++i_iter ;
    }
  
  // creates a bias field
  typedef itk::MultivariateLegendrePolynomial BiasFieldType ;
  BiasFieldType::DomainSizeType biasSize(3) ;
  int biasDegree = 3 ;
  biasSize[0] = imageSize[0] ;
  biasSize[1] = imageSize[1] ;
  biasSize[2] = imageSize[2] ;
  BiasFieldType bias(biasSize.size(), 
                     biasDegree, // bias field degree 
                     biasSize) ;

  // generates the coefficients using the normal random variate generator.
  BiasFieldType::CoefficientArrayType 
    coefficients(bias.GetNumberOfCoefficients()) ;

  randomGenerator->Initialize( (int) 2003 ) ;
  for ( unsigned int i = 0 ; i < bias.GetNumberOfCoefficients() ; ++i )
    {
    coefficients[i] = ( randomGenerator->GetVariate() + 1 ) * 0.01 ;
    }
  bias.SetCoefficients(coefficients) ;

  // set the imageWithBias pixel values with imageSource pixel value +
  // bias.
  ImageIteratorType ib_iter( imageWithBias, 
                            imageWithBias->GetLargestPossibleRegion() ) ;

  BiasFieldType::SimpleForwardIterator b_iter( &bias ) ;
  i_iter.GoToBegin() ;
  float temp ;
  while ( !i_iter.IsAtEnd() )
    {
    temp = i_iter.Get() * (2 + b_iter.Get()) ;
    ib_iter.Set( temp ) ;
    ++i_iter ;
    ++ib_iter ;
    ++b_iter ;
    }

  // creates a bias correction filter and run it.
  typedef itk::MRIBiasFieldCorrectionFilter<ImageType, ImageType, MaskType> 
    FilterType ;

  FilterType::Pointer filter = FilterType::New() ;


  // To see the debug output for each iteration, uncomment the
  // following line. 
  filter->DebugOn() ;

  filter->SetInput( imageWithBias ) ;
  filter->IsBiasFieldMultiplicative( true ) ;
//  filter->SetInitialBiasFieldCoefficients( coefficients ) ;
  filter->SetBiasFieldDegree( biasDegree ) ;
  filter->SetTissueClassStatistics( classMeans, classSigmas ) ;
  filter->SetOptimizerGrowthFactor( 1.5 ) ;
  filter->SetOptimizerInitialRadius( 1.02 ) ;
  filter->SetVolumeCorrectionMaximumIteration( 1000 ) ;
  filter->SetUsingInterSliceIntensityCorrection( false ) ;
  filter->SetInterSliceCorrectionMaximumIteration( 200 ) ;
  filter->SetUsingSlabIdentification( false ) ;
  filter->SetSlicingDirection( 2 ) ;
  filter->Update() ;

  double sumOfError = 0.0 ;
  ImageIteratorType o_iter( filter->GetOutput(), 
                            filter->GetOutput()->GetLargestPossibleRegion() );
  i_iter.GoToBegin() ;
  while ( !i_iter.IsAtEnd() )
    {
    sumOfError += vnl_math_abs( o_iter.Get() - i_iter.Get() ) ;
    ++i_iter ;
    ++o_iter ;
    }

  b_iter.Begin() ;
  ImageIteratorType bias_iter( biasImage, imageRegion ) ;
  while ( !b_iter.IsAtEnd() )
    {
    bias_iter.Set( b_iter.Get() + 2 ) ;
    ++b_iter ;
    ++bias_iter ;
    }

  if ( SaveImages )
    {
    typedef itk::ImageFileWriter< ImageType > WriterType ;
    WriterType::Pointer writer = WriterType::New() ;
    
    writer->SetInput( image ) ;
    writer->SetFileName( "MRISource.mhd" ) ;
    writer->Update() ;
    
    WriterType::Pointer writer2 = WriterType::New() ;
    writer2->SetInput(imageWithBias) ;
    writer2->SetFileName( "MRISourceWithBias.mhd" ) ;
    writer2->Update() ;
    
    WriterType::Pointer writer3 = WriterType::New() ;
    writer3->SetInput(filter->GetOutput()) ;
    writer3->SetFileName( "MRICorrected.mhd" ) ;
    writer3->Update() ;
  
    WriterType::Pointer writer4 = WriterType::New() ;
    writer4->SetInput( biasImage ) ;
    writer4->SetFileName( "MRIBias.mhd" ) ;
    writer4->Update() ;
    }

  std::cout << "Avg. error = " 
            << sumOfError / (imageSize[0] * imageSize[1] * imageSize[2]) 
            << std::endl ;
  return EXIT_SUCCESS ;
}
