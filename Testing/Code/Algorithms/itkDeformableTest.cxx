/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDeformableTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <itkImage.h>
#include <itkFirstDerivativeRecursiveGaussianImageFilter.h>
#include <itkSecondDerivativeRecursiveGaussianImageFilter.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkSphereMeshSource.h>
#include <itkDeformableMesh3DFilter.h>
#include <itkMesh.h>
#include <time.h>
#include <itkGradientRecursiveGaussianImageFilter.h>
#include <itkCovariantVector.h>


int WIDTH = 50;
int HEIGHT = 50;
int DEPTH = 50;

/* seed point */
int SEEDX = 25;
int SEEDY = 25;
int SEEDZ = 25;


long btime, etime;
int main() 
{

  // Define the dimension of the images
  const unsigned int myDimension = 3;

  // Declare the types of the images
  typedef itk::Image<double, myDimension>           myImageType;

  // Declare the types of the output images
  typedef itk::Image<unsigned short, myDimension>  outImageType;

  // Declare the type of the index to access images
  typedef itk::Index<myDimension>       myIndexType;

  // Declare the type of the size 
  typedef itk::Size<myDimension>        mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension> myRegionType;

  // Declare the type of the Mesh
  typedef itk::Mesh<double>   DMesh;

  typedef DMesh::PointType   OPointType;

  // Declare the type of the gradient image
  typedef itk::CovariantVector<double, myDimension> myGradientType;
  typedef itk::Image<myGradientType, myDimension>  myGradientImageType;


  unsigned short *TestImage = new unsigned short[WIDTH*HEIGHT];
  unsigned char  *Test = new unsigned char[WIDTH*HEIGHT*DEPTH];


  typedef itk::SphereMeshSource<DMesh>               SphereSourceType;
  typedef itk::DeformableMesh3DFilter<DMesh, DMesh>  BFilter;
//  typedef itk::SphereSource<DMesh>            SphereSourceType;
//  typedef itk::DeformableMeshFilter<DMesh, DMesh>     BFilter;

  outImageType::Pointer ptimg=outImageType::New();
  myGradientImageType::Pointer gdimg=myGradientImageType::New();
  outImageType::Pointer outputimg = outImageType::New();
//  outImageType::Pointer gdimg=outImageType::New();
//  outImageType::Pointer outputimg = outImageType::New();

  outImageType::SizeType outsize={{WIDTH,HEIGHT,DEPTH}};
  outImageType::IndexType index=outImageType::IndexType::ZeroIndex;
  outImageType::RegionType outregion;
  outregion.SetSize(outsize);
  outregion.SetIndex(index);

  myGradientImageType::SizeType gdsize={{WIDTH,HEIGHT,DEPTH}};
  myGradientImageType::IndexType gdindex=myGradientImageType::IndexType::ZeroIndex;
  myGradientImageType::RegionType gdregion;
  gdregion.SetSize(gdsize);
  gdregion.SetIndex(gdindex);
  
  ptimg->SetLargestPossibleRegion( outregion );
  ptimg->SetBufferedRegion( outregion );
  ptimg->SetRequestedRegion( outregion );
  ptimg->Allocate();

  gdimg->SetLargestPossibleRegion( gdregion );
  gdimg->SetBufferedRegion( gdregion );
  gdimg->SetRequestedRegion( gdregion );
  gdimg->Allocate();

  outputimg->SetLargestPossibleRegion( outregion );
  outputimg->SetBufferedRegion( outregion );
  outputimg->SetRequestedRegion( outregion );
  outputimg->Allocate();

  // Create the image
  myImageType::Pointer inputImage  = myImageType::New();

  mySizeType size={{WIDTH,HEIGHT,DEPTH}};
  myIndexType start;
  start = myIndexType::ZeroIndex;

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


  while( !it.IsAtEnd() ) 
  {
    it.Set( 0.0 );
    ++it;
  }

  size[0] = 30;
  size[1] = 30;
  size[2] = 30;

  start[0] = 10;
  start[1] = 10;
  start[2] = 10;

  // Create one iterator for an internal region
  region.SetSize( size );
  region.SetIndex( start );
  itk::ImageRegionIteratorWithIndex <myImageType> itb( inputImage, region );

  // Initialize the content the internal region
  while( !itb.IsAtEnd() ) 
  {
    itb.Set( 100.0 );
    ++itb;
  }

  typedef itk::GradientRecursiveGaussianImageFilter<
                                            myImageType,
                                            myGradientImageType,
                                            double       >  myFilterType;
            
  // Create a  Filter                                
  myFilterType::Pointer filter = myFilterType::New();


  // Connect the input images
  filter->SetInput( inputImage ); 

  // Set sigma
  filter->SetSigma( 2.0 );
  
  // Execute the filter
  filter->Update();

  itk::ImageRegionIteratorWithIndex <myGradientImageType> outit(filter->GetOutput(), gdregion);
  outit.GoToBegin();

  std::cout << " Done !" << std::endl;

// allocating the input image data.
  BFilter::Pointer m_bfilter = BFilter::New();
  SphereSourceType::Pointer m_spheresource = SphereSourceType::New();
  m_bfilter->SetInput(m_spheresource->GetOutput());

  OPointType m_spherecenter;
  OPointType m_scale;
  m_spherecenter[0] = (double) SEEDX;
  m_spherecenter[1] = (double) SEEDY;
  m_spherecenter[2] = (double) SEEDZ;
  m_scale[0] = 5;
  m_scale[1] = 5;
  m_scale[2] = 5;
  m_spheresource->SetCenter(m_spherecenter);
  m_spheresource->SetResolutionX(10);
  m_spheresource->SetResolutionY(30);
  m_spheresource->SetSquareness1(0.5);
  m_spheresource->SetSquareness2(0.5);
  m_spheresource->SetScale(m_scale);
  m_spheresource->Update();

  myIndexType m_center={{SEEDX, SEEDY, SEEDZ}};
  m_bfilter->SetCenter(m_center);
  m_bfilter->SetNeighborRadius(5);
  m_bfilter->SetStiffnessV(0.00001);
  m_bfilter->SetStiffnessH(0.04);
  m_bfilter->SetTimeStep(0.001);
  m_bfilter->SetZDistance(1.0);
  itk::ImageRegionIteratorWithIndex <outImageType> ptit(ptimg, outregion);
  
  ptit.GoToBegin();
  while( !ptit.IsAtEnd() ) 
  {
    ptit.Set( 0 );
    ++ptit;
  }

  outsize[0] = 30;
  outsize[1] = 30;
  outsize[2] = 30;

  index[0] = 10;
  index[1] = 10;
  index[2] = 10;

  // Create one iterator for an internal region
  outregion.SetSize( outsize );
  outregion.SetIndex( index );
  itk::ImageRegionIteratorWithIndex <outImageType> ptitb(ptimg, outregion);

  // Initialize the content the internal region
  ptitb.GoToBegin();
  while( !ptitb.IsAtEnd() ) 
  {
    ptitb.Set( 100 );
    ++ptitb;
  }

  outsize[0] = 50;
  outsize[1] = 50;
  outsize[2] = 50;

  index[0] = 0;
  index[1] = 0;
  index[2] = 0;

  outregion.SetSize(outsize);
  outregion.SetIndex(index);

  itk::ImageRegionIteratorWithIndex <myGradientImageType> gdit(gdimg, gdregion);

  gdit.GoToBegin();
  outit.GoToBegin();
  while( !gdit.IsAtEnd()) { 
    gdit.Set(outit.Get());
    ++gdit;
    ++outit;
  }

  m_bfilter->SetPotential(ptimg);
  m_bfilter->SetGradient(gdimg);
  m_bfilter->SetImageOutput(outputimg);
  m_bfilter->SetXResolution(10);
  m_bfilter->SetYResolution(30);
  m_bfilter->SetZResolution(1);
  m_bfilter->SetStepThreshold(100);
  m_bfilter->SetSliceDistanceThreshold(1.0);
  m_bfilter->SetModelDistanceToBoundaryThreshold(0.0);

  time(&btime);
  m_bfilter->Update();
  time(&etime);

  std::cout<<"Finished: "<<etime-btime<<" seconds."<<std::endl;
  
// All objects should be automatically destroyed at this point
  return 0;

}




