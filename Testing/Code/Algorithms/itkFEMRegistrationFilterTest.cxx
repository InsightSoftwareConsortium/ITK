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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <fstream>
#include "itkFEMRegistrationFilter.h"
#include "itkFEMImageMetricLoadImplementation.h"

#include "itkIndex.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkCommand.h"
#include "vnl/vnl_math.h"

// tyepdefs necessary for FEM visitor dispatcher

  typedef unsigned char PixelType;
  typedef itk::Image<PixelType,3> testImageType;
  typedef itk::fem::Element3DC0LinearHexahedronMembrane   ElementType;
//  typedef itk::fem::Element2DC0LinearQuadrilateralMembrane   ElementType;

//#ifdef  USEIMAGEMETRIC
  typedef itk::fem::ImageMetricLoad<testImageType,testImageType>     ImageLoadType2;
//#else
  typedef itk::fem::FiniteDifferenceFunctionLoad<testImageType,testImageType>     ImageLoadType;
//#endif
  template class itk::fem::ImageMetricLoadImplementation<ImageLoadType>;
  typedef ElementType::LoadImplementationFunctionPointer     LoadImpFP;
  typedef ElementType::LoadType                              ElementLoadType;
  typedef itk::fem::VisitorDispatcher<ElementType,ElementLoadType, LoadImpFP>
                                                          DispatcherType;

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

  const unsigned int ImageDimension = 3;
  typedef itk::Vector<float,ImageDimension> VectorType;
  typedef itk::Image<VectorType,ImageDimension> FieldType;
  typedef itk::Image<VectorType::ValueType,ImageDimension> FloatImageType;
  typedef testImageType::IndexType  IndexType;
  typedef testImageType::SizeType   SizeType;
  typedef testImageType::RegionType RegionType;

  //--------------------------------------------------------
  std::cout << "Generate input images and initial deformation field";
  std::cout << std::endl;

  FloatImageType::SizeValueType sizeArray[ImageDimension] = { 32, 32, 32 };

  SizeType size;
  size.SetSize( sizeArray );

  IndexType index;
  index.Fill( 0 );

  RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  testImageType::Pointer moving = testImageType::New();
  testImageType::Pointer fixed = testImageType::New();
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
  center[0] = 16; center[1] = 16; center[2] = 16;radius = 5;
  FillWithCircle<testImageType>( moving, center, radius, fgnd, bgnd );

  // fill fixed with circle
  center[0] = 16; center[1] = 16; center[2] = 16; radius = 8;
  FillWithCircle<testImageType>( fixed, center, radius, fgnd, bgnd );

  // fill initial deformation with zero vectors
  VectorType zeroVec;
  zeroVec.Fill( 0.0 );
  FillImage<FieldType>( initField, zeroVec );

  //-------------------------------------------------------------
  std::cout << "Run registration and warp moving" << std::endl;


  // register the elements with visitor dispatcher
  {
    typedef itk::fem::ImageMetricLoadImplementation<ImageLoadType> iml;
    ElementType::LoadImplementationFunctionPointer fp = &iml::ImplementImageMetricLoad;
    DispatcherType::RegisterVisitor((ImageLoadType*)0,fp);
  }

  for (int met=0; met<6; met++)
    {

    typedef itk::fem::FEMRegistrationFilter<testImageType,testImageType> RegistrationType;
    RegistrationType::Pointer registrator = RegistrationType::New();
    registrator->SetFixedImage( fixed );
    registrator->SetMovingImage( moving );

    registrator->DoMultiRes(true);
    registrator->SetNumLevels(1);
    registrator->SetMaxLevel(1);
    registrator->SetMovingImage( moving );
    registrator->SetFixedImage( fixed );
//  registrator->SetTemp(1.0);
    registrator->ChooseMetric((float)met);
    unsigned int maxiters=5;
    float e=1.e6;
    float p=1.e5;
//  std::cout << " input num iters, e, p: ";  std::cin >> maxiters >> e >> p;
    registrator->SetElasticity(e,0);
    registrator->SetRho(p,0);
    registrator->SetGamma(1.,0);
    registrator->SetAlpha(1.);
    registrator->SetMaximumIterations( maxiters,0 );
    registrator->SetMeshPixelsPerElementAtEachResolution(4,0);
    registrator->SetWidthOfMetricRegion(0 ,0);
    if ( met == 0 || met == 5)
      registrator->SetWidthOfMetricRegion(0 ,0);
    else
      registrator->SetWidthOfMetricRegion(1 ,0);
    registrator->SetNumberOfIntegrationPoints(2,0);
    registrator->SetDescentDirectionMinimize();
    registrator->SetDescentDirectionMaximize();
    registrator->DoLineSearch(false);
    registrator->SetTimeStep(1.);
    if (met == 0)
      {
      registrator->DoLineSearch((int)2);
      registrator->EmployRegridding(true);
      }
    else
      {
      registrator->DoLineSearch((int)0);
      registrator->EmployRegridding(false);
      }
    registrator->UseLandmarks(false);

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
    ElementType::Pointer e1 = ElementType::New();
    e1->m_mat=dynamic_cast<itk::fem::MaterialLinearElasticity*>( m );
    registrator->SetElement(e1);
    registrator->SetMaterial(m);

    registrator->Print( std::cout );


    try
      {
      // Register the images
      registrator->RunRegistration();
      }
    catch(... )
      {
      //fixme - changes to femparray cause it to fail : old version works
      std::cout << "Caught an exception: " << std::endl;
      delete e1;
      delete m;
      return EXIT_FAILURE;
      //    std::cout << err << std::endl;
      //throw err;
      }
    delete e1;
    delete m;

    }

  /*
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
  */

  return EXIT_SUCCESS;


}

