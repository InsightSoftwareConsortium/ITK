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

#include "itkDeformableMesh3DFilter.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkGradientImageFilter.h"
#include "itkGradientToMagnitudeImageFilter.h"
#include "itkGradientVectorFlowImageFilter.h"
#include "itkShrinkImageFilter.h"
#include "itkBinaryMask3DMeshSource.h"

int itkDeformableTest(int , char *[])
{
  const itk::Size<3u>::SizeValueType WIDTH = 32;
  const itk::Size<3u>::SizeValueType HEIGHT = 32;
  const itk::Size<3u>::SizeValueType DEPTH = 32;

  // Define the dimension of the images
  const unsigned int myDimension = 3;

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
  typedef itk::CovariantVector<double, 2>           double2DVector;
  typedef itk::CovariantVector<double, 3>           double3DVector;

  typedef itk::BinaryMask3DMeshSource<binaryImageType,DMesh>  myMeshSource;
  typedef itk::LaplacianImageFilter<myImageType, myImageType> myLaplacianFilterType;
  typedef itk::GradientVectorFlowImageFilter<myGradientImageType, myGradientImageType>
                                              myGVFFilterType;

  typedef itk::GradientImageFilter<myImageType, double, double>
                                              myGFilterType;

  typedef itk::GradientToMagnitudeImageFilter<myGradientImageType, myImageType>
                                              myGToMFilterType;

  typedef itk::DeformableMesh3DFilter<DMesh, DMesh>  DFilter;

  binaryImageType::Pointer       biimg=binaryImageType::New();
  myGradientImageType::Pointer   gdimg=myGradientImageType::New();

  binaryImageType::SizeType      bisize={{WIDTH,HEIGHT,DEPTH}};
  binaryImageType::IndexType     biindex;
  binaryImageType::RegionType    biregion;
  biindex.Fill(0);
  biregion.SetSize(bisize);
  biregion.SetIndex(biindex);

  myGradientImageType::SizeType   gdsize={{WIDTH,HEIGHT,DEPTH}};
  myGradientImageType::IndexType  gdindex;
  myGradientImageType::RegionType gdregion;
  gdindex.Fill(0);
  gdregion.SetSize(gdsize);
  gdregion.SetIndex(gdindex);

  biimg->SetLargestPossibleRegion( biregion );
  biimg->SetBufferedRegion( biregion );
  biimg->SetRequestedRegion( biregion );
  biimg->Allocate();

  gdimg->SetLargestPossibleRegion( gdregion );
  gdimg->SetBufferedRegion( gdregion );
  gdimg->SetRequestedRegion( gdregion );
  gdimg->Allocate();

  // Create the image
  myImageType::Pointer inputImage  = myImageType::New();

  mySizeType size={{WIDTH,HEIGHT,DEPTH}};
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


  while( !it.IsAtEnd() )
  {
    it.Set( 0.0 );
    bit.Set( 0 );
    ++it;
    ++bit;
  }

  size[0] = 16;
  size[1] = 16;
  size[2] = 16;

  start[0] = 8;
  start[1] = 8;
  start[2] = 8;

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

  itk::ShrinkImageFilter< myImageType, myImageType >::Pointer dshrink;
  dshrink = itk::ShrinkImageFilter< myImageType, myImageType >::New();
  dshrink->SetInput( inputImage );
  dshrink->SetNumberOfThreads(4);

  unsigned int dfactors[3] = { 1, 1, 1 };
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
  grfilter->SetSigma( 1.0 );

  myLaplacianFilterType::Pointer m_LFilter = myLaplacianFilterType::New();
  myGVFFilterType::Pointer m_GVFFilter = myGVFFilterType::New();

  m_GVFFilter->SetInput(gfilter->GetOutput());
  m_GVFFilter->SetLaplacianFilter(m_LFilter);
  m_GVFFilter->SetNoiseLevel(500);

  gtomfilter->SetInput(grfilter->GetOutput());

  gfilter->SetInput(gtomfilter->GetOutput());
  gfilter->Update();

  std::cout << "The gradient map created!" << std::endl;

//  the gvf is temproraily disabled since the problem related with gradientimagefilter
//  m_GVFFilter->Update();

//  std::cout << "GVF created! " << std::endl;

////////////////////////////////////////////////////////////////////////////////////////
// construct the deformable mesh

  itk::ShrinkImageFilter< binaryImageType, binaryImageType >::Pointer shrink;
  shrink = itk::ShrinkImageFilter< binaryImageType, binaryImageType >::New();
  shrink->SetInput( biimg );
  shrink->SetNumberOfThreads(4);

  unsigned int factors[3] = { 1, 1, 1 };
  shrink->SetShrinkFactors(factors);
  shrink->UpdateLargestPossibleRegion();

  binaryImageType::RegionType requestedRegion;
  requestedRegion = shrink->GetOutput()->GetRequestedRegion();

  myMeshSource::Pointer m_bmmeshsource = myMeshSource::New();

  DFilter::Pointer m_dfilter = DFilter::New();
  m_dfilter->SetInput(m_bmmeshsource->GetOutput());
//  m_dfilter->SetGradient(m_GVFFilter->GetOutput());
  m_dfilter->SetGradient(gfilter->GetOutput());

  m_bmmeshsource->SetInput( shrink->GetOutput() );
  m_bmmeshsource->SetObjectValue( 255 );
  m_bmmeshsource->Update();

  std::cout << "Deformable mesh created using Marching Cube!" << std::endl;

  double2DVector m_stiff;
  m_stiff[0] = 0.0001;
  m_stiff[1] = 0.1;

  double3DVector m_scale;
  m_scale[0] = 1;
  m_scale[1] = 1;
  m_scale[2] = 1;
  m_dfilter->SetStiffness(m_stiff);
  m_dfilter->SetGradientMagnitude(1.0);
  m_dfilter->SetTimeStep(0.01);
  m_dfilter->SetStepThreshold(10);
  m_dfilter->SetScale(m_scale);
  m_dfilter->SetObjectLabel(1);
  m_dfilter->SetPotentialOn(0);
  m_dfilter->SetPotentialMagnitude(1.0);

  std::cout << "Deformable mesh fitting...";
  m_dfilter->Update();

  /** raise coverage */
  myGradientImageType::Pointer grad_tmp = m_dfilter->GetGradient();
  std::cout << grad_tmp << std::endl;
  std::cout << m_dfilter->GetStepThreshold() << std::endl;

  double2DVector stiff_tmp = m_dfilter->GetStiffness();
  std::cout << stiff_tmp <<std::endl;

  std::cout << m_dfilter->GetTimeStep() << std::endl;

  DMesh::Pointer norm_tmp = m_dfilter->GetNormals();
  std::cout << norm_tmp << std::endl;
  std::cout << m_dfilter;

  std::cout << "Mesh Source: " << m_bmmeshsource;

// All objects should be automatically destroyed at this point

  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;

}
