/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBallonForce3DFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <math.h>
#include <iostream>
#include <time.h>

#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkBalloonForce3DFilter.h"
#include "itkMesh.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkSphereMeshSource.h"


int itkBallonForce3DFilterTest(int, char**)
{
  
  // Define the dimension of the images
  const unsigned int myDimension = 3;

  // Declare the types of the images
  typedef itk::Image<double, myDimension>       myImageType;

  // Declare the type of the Mesh
  typedef itk::Mesh<double>                     MeshType;

  // Declare the type of the gradient image
  typedef itk::CovariantVector<double, myDimension> myGradientType;
  typedef itk::Image<myGradientType, myDimension>   myGradientImageType;
  typedef itk::BalloonForce3DFilter<MeshType, MeshType>  BallonForceFilterType;
  typedef itk::SphereMeshSource< MeshType >                  MeshSourceType;
  typedef itk::ImageRegionIteratorWithIndex<myImageType>  myIteratorType;

  typedef itk::GradientRecursiveGaussianImageFilter< myImageType, 
                                                     myGradientImageType >  myFilterType;

  MeshSourceType::Pointer meshSource = MeshSourceType::New();
  meshSource->SetResolutionX( 20 );
  meshSource->SetResolutionY( 20 );
  
  // Create the image
  myImageType::Pointer inputImage  = myImageType::New();

  myImageType::SizeType size;
  size[0] = 100;
  size[1] = 100;
  size[2] = 100;

  myImageType::IndexType start;
  start.Fill(0);

  myImageType::RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputImage->SetRegions( region );
  inputImage->Allocate();

  myIteratorType it( inputImage, region );
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    it.Set( 0.0 );
    ++it;
    }

            
  size[0] = 50;
  size[1] = 50;
  size[2] = 50;

  start[0] = 25;
  start[1] = 25;
  start[2] = 25;

  region.SetIndex( start );
  region.SetSize( size );

  // Set a different value inside
  myIteratorType itn( inputImage, region );
  itn.GoToBegin();
  while( !it.IsAtEnd() )
    {
    itn.Set( 200.0 );
    ++itn;
    }



  // Create a  Filter                                
  myFilterType::Pointer  gradientFilter = myFilterType::New();

  // Connect the input images
  gradientFilter->SetInput( inputImage );
  gradientFilter->SetSigma( 2.0 );

  try
    {
    gradientFilter->Update();
    }
  catch( itk::ExceptionObject & exp )
    {
    std::cerr << "ITK Exception thrown while updating GradientFilter"  << std::endl;
    std::cerr << exp << std::endl;
    }

  try
    {
    meshSource->Update();
    }
  catch( itk::ExceptionObject & exp )
    {
    std::cerr << "ITK Exception thrown while updating SphereMeshSource"  << std::endl;
    std::cerr << exp << std::endl;
    }


  BallonForceFilterType::Pointer ballonFilter = BallonForceFilterType::New();
  ballonFilter->SetInput( meshSource->GetOutput() ); 
  ballonFilter->SetGradient(  gradientFilter->GetOutput() );

  ballonFilter->SetStiffnessV( 0.00001 );
  ballonFilter->SetStiffnessH( 0.00001 );
  ballonFilter->SetTimeStep( 0.2 );
  ballonFilter->SetStepThreshold1( 1 );
  ballonFilter->SetStepThreshold2( 2 );

  try
    {
    ballonFilter->Update();
    }
  catch( itk::ExceptionObject & exp )
    {
    std::cerr << "ITK Exception thrown while updating BallonForce3DFilter"  << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }
  catch( std::exception & stdexp )
    {
    std::cerr << "STL Exception thrown while updating BallonForce3DFilter"  << std::endl;
    std::cerr << stdexp.what() << std::endl;
    return EXIT_FAILURE;
    }
  catch( ... )
    {
    std::cerr << "Unknown Exception thrown while updating BallonForce3DFilter"  << std::endl;
    return EXIT_FAILURE;
    }
 

// All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;

}
