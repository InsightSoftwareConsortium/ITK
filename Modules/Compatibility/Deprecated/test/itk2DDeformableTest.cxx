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

#include <iostream>

#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkGradientImageFilter.h"
#include "itkGradientToMagnitudeImageFilter.h"
#include "itkShrinkImageFilter.h"
#include "itkBalloonForceFilter.h"
#include "itkSphereMeshSource.h"


int itk2DDeformableTest(int, char* [])
{
  // change the image size to your test images
  const itk::Size<2u>::SizeValueType WIDTH = 100;
  const itk::Size<2u>::SizeValueType HEIGHT = 100;

  // Define the dimension of the images
  const unsigned int myDimension = 2;

  // Declare the types of the images
  typedef itk::Image<double, myDimension>           myImageType;

  // Declare the types of the output images
  typedef itk::Image<unsigned short, myDimension>   binaryImageType;

  // Declare the type of the index to access images
  typedef itk::Index<myDimension>       myIndexType;

  // Declare the type of the size
  typedef itk::Size<myDimension>        mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension> myRegionType;

  // Declare the type of the Mesh
  typedef itk::Mesh<double>   DMesh;

  // Declare the type of the gradient image
  typedef itk::CovariantVector<double, myDimension> myGradientType;
  typedef itk::Image<myGradientType, myDimension>   myGradientImageType;
  typedef itk::Vector<double, 2>                    double2DVector;

  typedef itk::SphereMeshSource<DMesh>  myMeshSource;

  typedef itk::GradientImageFilter<myImageType, double, double>
                                              myGFilterType;

  typedef itk::GradientToMagnitudeImageFilter<myGradientImageType, myImageType>
                                              myGToMFilterType;

  typedef itk::BalloonForceFilter<DMesh, DMesh>  DFilter;

  binaryImageType::Pointer       biimg=binaryImageType::New();

  binaryImageType::SizeType      bisize={{WIDTH,HEIGHT}};
  binaryImageType::IndexType     biindex;
  binaryImageType::RegionType    biregion;
  biindex.Fill(0);
  biregion.SetSize(bisize);
  biregion.SetIndex(biindex);

  biimg->SetLargestPossibleRegion( biregion );
  biimg->SetBufferedRegion( biregion );
  biimg->SetRequestedRegion( biregion );
  biimg->Allocate();

  // Create the image
  myImageType::Pointer inputImage  = myImageType::New();

  mySizeType size={{WIDTH,HEIGHT}};
  myIndexType start;
  start.Fill(0);

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  itk::ImageRegionIteratorWithIndex <myImageType> it(inputImage, region);
  it.GoToBegin();
  itk::ImageRegionIteratorWithIndex <binaryImageType> bit(biimg, biregion);
  bit.GoToBegin();

  /////////////////////////////////////////////////////////////////////////
  // create user defined images for test


  while( !it.IsAtEnd() )
  {
    it.Set( 0.0 );
    bit.Set( 0 );
    ++it;
    ++bit;
  }

  size[0] = 60;
  size[1] = 60;

  start[0] = 20;
  start[1] = 20;

  // Create one iterator for an internal region
  region.SetSize( size );
  region.SetIndex( start );
  biregion.SetSize( size );
  biregion.SetIndex( start );
  itk::ImageRegionIteratorWithIndex <myImageType> itb( inputImage, region );
  itk::ImageRegionIteratorWithIndex <binaryImageType> bitb( biimg, biregion );

  // Initialize the content the internal region
  while( !itb.IsAtEnd() )
  {
    itb.Set( 100.0 );
    bitb.Set ( 255 );
    ++itb;
    ++bitb;
  }

  //////////////////////////////////////////////////////////////////////////
  // calculate gradient map

  itk::ShrinkImageFilter< myImageType, myImageType >::Pointer dshrink;
  dshrink = itk::ShrinkImageFilter< myImageType, myImageType >::New();
  dshrink->SetInput( inputImage );
  dshrink->SetNumberOfThreads(4);

  unsigned int dfactors[2] = { 1, 1 };
  dshrink->SetShrinkFactors(dfactors);
  dshrink->UpdateLargestPossibleRegion();

  myImageType::RegionType drequestedRegion;
  drequestedRegion = dshrink->GetOutput()->GetRequestedRegion();

  typedef itk::GradientRecursiveGaussianImageFilter<
                                            myImageType,
                                            myGradientImageType
                                            >  myFilterType;


  // Create a  Filter
  myFilterType::Pointer grfilter = myFilterType::New();
  myGFilterType::Pointer gfilter = myGFilterType::New();
  myGToMFilterType::Pointer gtomfilter = myGToMFilterType::New();

  // Connect the input images
  grfilter->SetInput( dshrink->GetOutput() );

  // Set sigma
  grfilter->SetSigma( 3.0 );

  gtomfilter->SetInput(grfilter->GetOutput());
  gfilter->SetInput(gtomfilter->GetOutput());

  gfilter->Update();

  std::cout << "The gradient map created!" << std::endl;

////////////////////////////////////////////////////////////////////////////////////////
// construct the deformable mesh

  myMeshSource::Pointer m_bmmeshsource = myMeshSource::New();

  itk::Mesh<float, 3>::PointType meshcenter;

  meshcenter[0] = 50;
  meshcenter[1] = 50;
  meshcenter[2] = 0;

  m_bmmeshsource->SetCenter(meshcenter);

  m_bmmeshsource->SetResolutionX(1);
  m_bmmeshsource->SetResolutionY(100);

////////////////////////////////////////////////////////////////////////////////////////
// deformable mesh fitting

  DFilter::Pointer m_dfilter = DFilter::New();
  m_dfilter->SetInput(m_bmmeshsource->GetOutput());
  m_dfilter->SetGradient(gfilter->GetOutput());

  double2DVector m_stiff;
  myIndexType  modelcenter;
  m_stiff[0] = 0.001;
  m_stiff[1] = 1;
  modelcenter[0] = 50;
  modelcenter[1] = 50;
  m_dfilter->SetCenter(modelcenter);
  m_dfilter->SetPotential(biimg);

  m_dfilter->SetStiffness(m_stiff);
  m_dfilter->SetTimeStep(0.0185);
  m_dfilter->SetDistanceForGradient(1.0);
  m_dfilter->SetDistanceToStop(6.0);
  m_dfilter->SetResolution(100);
  m_dfilter->SetGradientBegin(0);
  std::cout << "Deformable mesh fitting...";

  m_dfilter->Update();
  std::cout << m_dfilter;

  DMesh::Pointer normals = m_dfilter->GetNormals();
  DMesh::Pointer locations = m_dfilter->GetLocations();
  DMesh::Pointer displacements = m_dfilter->GetDisplacements();
  DMesh::Pointer derives = m_dfilter->GetDerives();
  DMesh::Pointer forces = m_dfilter->GetForces();
  std::cout << normals << " " << locations << " "
            << displacements << " " << derives << " " << forces << std::endl;

  DMesh::PointsContainerPointer     points;
  DMesh::CellsContainerPointer      cells;
  DMesh::PointType                  node;

  std::cout << "Mesh Source: " << m_bmmeshsource;

// All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
