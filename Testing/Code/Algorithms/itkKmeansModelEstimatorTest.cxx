/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKmeansModelEstimatorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
// Insight classes
#include "itkImage.h"
#include "itkVector.h"
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/vnl_math.h"
#include "itkImageRegionIterator.h"
#include "itkLightProcessObject.h"

#include "itkImageKmeansModelEstimator.h"
#include "itkDistanceToCentroidMembershipFunction.h"

//Data definitions 
#define   IMGWIDTH            16
#define   IMGHEIGHT           1
#define   NFRAMES             1
#define   NUMBANDS            2
#define   NDIMENSION          3

#define   CDBKWIDTH           4
#define   CDBKHEIGHT          1
#define   NFRAMES             1
#define   NCODEWORDS          CDBKWIDTH * CDBKHEIGHT * NFRAMES
#define   NUMBANDS            2
#define   NDIMENSION          3
#define   STARTFRAME          0
#define   NUM_BYTES_PER_PIXEL 1
#define   ONEBAND             1


// class to support progress feeback
class ShowProgressObject
{
public:
  ShowProgressObject(itk::LightProcessObject * o)
    {m_Process = o;}
  void ShowProgress()
    {std::cout << "Progress " << m_Process->GetProgress() << std::endl;}
  itk::LightProcessObject::Pointer m_Process;
};

int itkKmeansModelEstimatorTest(int, char* [] )
{
  //------------------------------------------------------
  //Create a simple test vector with 16 entries and 2 bands
  //------------------------------------------------------
  typedef itk::Image<itk::Vector<double,NUMBANDS>,NDIMENSION> VecImageType; 

  typedef VecImageType::PixelType VecImagePixelType;
  
  VecImageType::Pointer vecImage = VecImageType::New();

  VecImageType::SizeType vecImgSize = {{ IMGWIDTH , IMGHEIGHT, NFRAMES }};

  VecImageType::IndexType index;
  index.Fill(0);
  VecImageType::RegionType region;

  region.SetSize( vecImgSize );
  region.SetIndex( index );

  vecImage->SetLargestPossibleRegion( region );
  vecImage->SetBufferedRegion( region );
  vecImage->Allocate();

  // setup the iterators
  typedef VecImageType::PixelType VecPixelType;

  enum { VecImageDimension = VecImageType::ImageDimension };
  typedef
    itk::ImageRegionIterator< VecImageType > VecIterator;

  VecIterator outIt( vecImage, vecImage->GetBufferedRegion() );

  //--------------------------------------------------------------------------
  //Manually create and store each vector
  //--------------------------------------------------------------------------

  //Vector no. 1
  VecPixelType vec;
  vec[0] = 21; vec[1] = 9; outIt.Set( vec ); ++outIt;
  //Vector no. 2
  vec[0] = 10; vec[1] = 20; outIt.Set( vec ); ++outIt;
  //Vector no. 3
  vec[0] = 8; vec[1] = 21; outIt.Set( vec ); ++outIt;
  //Vector no. 4
  vec[0] = 10; vec[1] = 23; outIt.Set( vec ); ++outIt;
  //Vector no. 5
  vec[0] = 12; vec[1] = 21; outIt.Set( vec ); ++outIt;
  //Vector no. 6
  vec[0] = 11; vec[1] = 12; outIt.Set( vec ); ++outIt;
  //Vector no. 7
  vec[0] = 15; vec[1] = 22; outIt.Set( vec ); ++outIt;
  //Vector no. 8
  vec[0] = 9; vec[1] = 10; outIt.Set( vec ); ++outIt;
  //Vector no. 9
  vec[0] = 19; vec[1] = 10; outIt.Set( vec ); ++outIt;
  //Vector no. 10
  vec[0] = 19; vec[1] = 10; outIt.Set( vec ); ++outIt;
  //Vector no. 11
  vec[0] = 21; vec[1] = 21; outIt.Set( vec ); ++outIt;
  //Vector no. 12
  vec[0] = 11; vec[1] = 20; outIt.Set( vec ); ++outIt;
  //Vector no. 13
  vec[0] = 8; vec[1] = 18; outIt.Set( vec ); ++outIt;
  //Vector no. 14
  vec[0] = 18; vec[1] = 10; outIt.Set( vec ); ++outIt;
  //Vector no. 15
  vec[0] = 22; vec[1] = 10; outIt.Set( vec ); ++outIt;
  //Vector no. 16
  vec[0] = 24; vec[1] = 23; outIt.Set( vec ); ++outIt;

  //---------------------------------------------------------------
  //Input the codebook
  //---------------------------------------------------------------
  //------------------------------------------------------------------
  //Read the codebook into an vnl_matrix
  //------------------------------------------------------------------

  vnl_matrix<double> inCDBK(NCODEWORDS, NUMBANDS);
  //There are 4 entries to the code book
  int r,c;
  r=0; c=0; inCDBK.put(r,c,10);
  r=0; c=1; inCDBK.put(r,c,10);
  r=1; c=0; inCDBK.put(r,c,10);
  r=1; c=1; inCDBK.put(r,c,20);
  r=2; c=0; inCDBK.put(r,c,20);
  r=2; c=1; inCDBK.put(r,c,10);
  r=3; c=0; inCDBK.put(r,c,20);
  r=3; c=1; inCDBK.put(r,c,20);

  //----------------------------------------------------------------------
  // Test code for the Kmeans model estimator
  //----------------------------------------------------------------------

  //---------------------------------------------------------------------
  // Multiband data is now available in the right format
  //---------------------------------------------------------------------

  //----------------------------------------------------------------------
  //Set membership function (Using the statistics objects)
  //----------------------------------------------------------------------
  namespace stat = itk::Statistics;

  typedef stat::DistanceToCentroidMembershipFunction< VecImagePixelType > 
    MembershipFunctionType ;
  typedef MembershipFunctionType::Pointer MembershipFunctionPointer ;

  typedef std::vector< MembershipFunctionPointer > 
    MembershipFunctionPointerVector;


  //----------------------------------------------------------------------
  //Set the image model estimator
  //----------------------------------------------------------------------
  typedef itk::ImageKmeansModelEstimator<VecImageType, MembershipFunctionType> 
    ImageKmeansModelEstimatorType;

  ImageKmeansModelEstimatorType::Pointer 
    applyKmeansEstimator = ImageKmeansModelEstimatorType::New();

  //----------------------------------------------------------------------
  //Set the parameters of the clusterer
  //----------------------------------------------------------------------
  applyKmeansEstimator->SetInputImage(vecImage);
  applyKmeansEstimator->SetNumberOfModels(NCODEWORDS);
  applyKmeansEstimator->SetThreshold(0.01);
  applyKmeansEstimator->Update();
  applyKmeansEstimator->Print(std::cout);

  MembershipFunctionPointerVector membershipFunctions = 
    applyKmeansEstimator->GetMembershipFunctions(); 

  vnl_vector<double> kmeansResultForClass;  
  vnl_vector<double> referenceCodebookForClass;
  vnl_vector<double> errorForClass;
  double error =0;
  double meanCDBKvalue = 0;

  for(unsigned int classIndex=0; classIndex < membershipFunctions.size(); 
    classIndex++ )
    {
    kmeansResultForClass = membershipFunctions[classIndex]->GetCentroid();
    referenceCodebookForClass = inCDBK.get_row( classIndex);
    errorForClass = kmeansResultForClass - referenceCodebookForClass;

    for(int i = 0; i < NUMBANDS; i++)
      {
      error += vnl_math_abs(errorForClass[i]/referenceCodebookForClass[i]);
      meanCDBKvalue += referenceCodebookForClass[i];
      }

    }
  error /= NCODEWORDS*NUMBANDS;
  meanCDBKvalue /= NCODEWORDS*NUMBANDS; 

  if( error < 0.1 * meanCDBKvalue)
    std::cout << "Kmeans algorithm passed (without initial input)"<<std::endl;
  else
    std::cout << "Kmeans algorithm failed (without initial input)"<<std::endl;

  //Validation with no codebook/initial Kmeans estimate
  vnl_matrix<double> kmeansResult = applyKmeansEstimator->GetKmeansResults();

  applyKmeansEstimator->SetCodebook(inCDBK);
  applyKmeansEstimator->Update();
  applyKmeansEstimator->Print(std::cout);

   membershipFunctions = applyKmeansEstimator->GetMembershipFunctions(); 

  //Validation with initial Kmeans estimate provided as input by the user

  error =0;
  meanCDBKvalue = 0;
  const unsigned int test = membershipFunctions.size();
  for(unsigned int classIndex=0; classIndex < test; classIndex++ )
    {
    kmeansResultForClass = membershipFunctions[classIndex]->GetCentroid();
    referenceCodebookForClass = inCDBK.get_row( classIndex);
    errorForClass = kmeansResultForClass - referenceCodebookForClass;

    for(int i = 0; i < NUMBANDS; i++)
      {
      error += vnl_math_abs(errorForClass[i]/referenceCodebookForClass[i]);
      meanCDBKvalue += referenceCodebookForClass[i];
      }

    }

  error /= NCODEWORDS*NUMBANDS;
  meanCDBKvalue /= NCODEWORDS*NUMBANDS; 

  if( error < 0.1 * meanCDBKvalue)
    std::cout << "Kmeans algorithm passed (with initial input)"<<std::endl;
  else
    std::cout << "Kmeans algorithm failed (with initial input)"<<std::endl;

  return 0;
}
