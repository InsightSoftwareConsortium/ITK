/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMRegistrationFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <fstream>
#include "itkFEMRegistrationFilter.h"
#include "itkFEMImageMetricLoadImplementation.h"

#include "itkIndex.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkCommand.h"
#include "vnl/vnl_math.h"

// tyepdefs necessary for FEM visitor dispatcher
  
  typedef unsigned char PixelType;
  typedef itk::Image<PixelType,2> ImageType;  
  typedef itk::fem::Element2DC0LinearQuadrilateralMembrane   ElementType;
  typedef itk::fem::ImageMetricLoad<ImageType,ImageType>     ImageLoadType;
  template class itk::fem::ImageMetricLoadImplementation<ImageLoadType>;
  typedef ElementType::LoadImplementationFunctionPointer     LoadImpFP;
  typedef ElementType::LoadType                              ElementLoadType;
  typedef itk::fem::VisitorDispatcher<ElementType,ElementLoadType, LoadImpFP>   
                                                          DispatcherType;

namespace{

  
// The following class is used to support callbacks
// on the filter in the pipeline that follows later
class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject* o)
    {m_Process = o;}
  void ShowProgress()
    {std::cout << "Progress " << m_Process->GetProgress() << std::endl;}
  itk::ProcessObject::Pointer m_Process;
};
}

// Template function to fill in an image with a value
template <class TImage>
void
FillImage(
TImage * image,
typename TImage::PixelType value )
{

 typedef itk::ImageRegionIteratorWithIndex<TImage> Iterator;
 Iterator it( image, image->GetBufferedRegion() );
 it.Begin();
    
 for( ; !it.IsAtEnd(); ++it )
  {
   it.Set( value );
  }

}


// Template function to fill in an image with a circle.
template <class TImage>
void
FillWithCircle(
TImage * image,
double * center,
double radius,
typename TImage::PixelType foregnd,
typename TImage::PixelType backgnd )
{

  typedef itk::ImageRegionIteratorWithIndex<TImage> Iterator;
  Iterator it( image, image->GetBufferedRegion() );
  it.Begin();
    
  typename TImage::IndexType index;
  double r2 = vnl_math_sqr( radius );

  for( ; !it.IsAtEnd(); ++it )
    {
    index = it.GetIndex();
    double distance = 0;
    for( unsigned int j = 0; j < TImage::ImageDimension; j++ )
      {
      distance += vnl_math_sqr((double) index[j] - center[j]);
      }
    if( distance <= r2 ) it.Set( foregnd );
    else it.Set( backgnd ); 
    }

}


// Template function to copy image regions
template <class TImage>
void
CopyImageBuffer(
TImage *input,
TImage *output )
{
  typedef itk::ImageRegionIteratorWithIndex<TImage> Iterator;
  Iterator inIt( input, output->GetBufferedRegion() );
  Iterator outIt( output, output->GetBufferedRegion() );
  for( ; !inIt.IsAtEnd(); ++inIt, ++outIt )
    {
    outIt.Set( inIt.Get() );
    }

}

int itkFEMRegistrationFilterTest(int, char* [] )
{

  const unsigned int ImageDimension = 2;
  typedef itk::Vector<float,ImageDimension> VectorType;
  typedef itk::Image<VectorType,ImageDimension> FieldType;
  typedef itk::Image<VectorType::ValueType,ImageDimension> FloatImageType;
  typedef ImageType::IndexType  IndexType;
  typedef ImageType::SizeType   SizeType;
  typedef ImageType::RegionType RegionType;

  //--------------------------------------------------------
  std::cout << "Generate input images and initial deformation field";
  std::cout << std::endl;

  unsigned long sizeArray[ImageDimension] = { 128, 128 };
  SizeType size;
  size.SetSize( sizeArray );

  IndexType index;
  index.Fill( 0 );
 
  RegionType region;
  region.SetSize( size );
  region.SetIndex( index );
  
  ImageType::Pointer moving = ImageType::New();
  ImageType::Pointer fixed = ImageType::New();
  FieldType::Pointer initField = FieldType::New();

  moving->SetLargestPossibleRegion( region );
  moving->SetBufferedRegion( region );
  moving->Allocate();

  fixed->SetLargestPossibleRegion( region );
  fixed->SetBufferedRegion( region );
  fixed->Allocate();
  
  initField->SetLargestPossibleRegion( region );
  initField->SetBufferedRegion( region );
  initField->Allocate();

  double center[ImageDimension];
  double radius;
  PixelType fgnd = 250;
  PixelType bgnd = 15;

  // fill moving with circle 
  center[0] = 64; center[1] = 64; radius = 30;
  FillWithCircle<ImageType>( moving, center, radius, fgnd, bgnd );

  // fill fixed with circle
  center[0] = 62; center[1] = 64; radius = 32;
  FillWithCircle<ImageType>( fixed, center, radius, fgnd, bgnd );

  // fill initial deformation with zero vectors
  VectorType zeroVec;
  zeroVec.Fill( 0.0 );
  FillImage<FieldType>( initField, zeroVec );

  //-------------------------------------------------------------
  std::cout << "Run registration and warp moving" << std::endl;
  

  // register the elements with visitor dispatcher
  {
    ElementType::LoadImplementationFunctionPointer fp = &itk::fem::ImageMetricLoadImplementation<ImageLoadType>::ImplementImageMetricLoad;
    DispatcherType::RegisterVisitor((ImageLoadType*)0,fp);
  }

  typedef itk::fem::FEMRegistrationFilter<ImageType,ImageType> RegistrationType;
  RegistrationType::Pointer registrator = RegistrationType::New();

  registrator->SetReferenceImage( moving );
  registrator->SetTargetImage( fixed );
  registrator->ChooseMetric(0);
  unsigned int maxiters=5;  
  float e=1.e6;
  float p=1.e5;
//  std::cout << " input num iters, e, p: ";  std::cin >> maxiters >> e >> p;
  registrator->SetElasticity(e,0);
  registrator->SetRho(p,0);
  registrator->SetMaximumIterations( maxiters,0 );
  registrator->SetLineSearchFrequency(1);
  registrator->SetLineSearchMaximumIterations(100);
  registrator->SetMeshElementsPerDimensionAtEachResolution(8,0);
  registrator->SetWidthOfMetricRegion(3 ,0);
  registrator->SetNumberOfIntegrationPoints(10,0);
  registrator->SetDescentDirectionMinimize();

  itk::fem::MaterialLinearElasticity::Pointer m;
  m=itk::fem::MaterialLinearElasticity::New();
  m->GN=0;       // Global number of the material ///
  m->E=registrator->GetElasticity();  // Young modulus -- used in the membrane ///
  m->A=1.0;     // Crossection area ///
  m->h=1.0;     // Crossection area ///
  m->I=1.0;    // Moment of inertia ///
  m->nu=0.; //.0;    // poissons -- DONT CHOOSE 1.0!!///
  m->RhoC=1.0;
  
  // Create the element type 
  ElementType::Pointer e1=ElementType::New();
  e1->m_mat=dynamic_cast<itk::fem::MaterialLinearElasticity*>( m );
  registrator->SetElement(e1);
  registrator->SetMaterial(m);

  registrator->Print( std::cout );

  ShowProgressObject progressWatch(registrator);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  registrator->AddObserver( itk::ProgressEvent(), command);
 
  registrator->RunRegistration();

  // get warped reference image
  // ---------------------------------------------------------
  std::cout << "Compare warped moving and fixed." << std::endl;

  // compare the warp and fixed images
  itk::ImageRegionIterator<ImageType> fixedIter( fixed,
      fixed->GetBufferedRegion() );
  itk::ImageRegionIterator<ImageType> warpedIter( registrator->GetWarpedImage(),
      fixed->GetBufferedRegion() );

  unsigned int numPixelsDifferent = 0;
  while( !fixedIter.IsAtEnd() )
    {
    if( fixedIter.Get() != warpedIter.Get() )
      {
      numPixelsDifferent++;
      }
    ++fixedIter;
    ++warpedIter;
    }

  std::cout << "Number of pixels different: " << numPixelsDifferent; 
  std::cout << std::endl;

  if( numPixelsDifferent > 400 )
    {
    std::cout << "Test failed - too many pixels different." << std::endl;
    return EXIT_FAILURE;
    }


  
  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;
  

}

