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

// Insight classes

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
  enum { VecImageDimension = VecImageType::ImageDimension };
  typedef
    itk::ImageRegionIterator< VecImageType > VecIterator;

  VecIterator outIt( vecImage, vecImage->GetBufferedRegion() );

  //--------------------------------------------------------------------------
  //Manually create and store each vector
  //--------------------------------------------------------------------------

  //Vector no. 1
  VecImagePixelType vec;
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

  outIt.GoToBegin();

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
                                          MembershipFunctionType;
  typedef MembershipFunctionType::Pointer MembershipFunctionPointer;

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
  applyKmeansEstimator->SetThreshold(0.01 );
  applyKmeansEstimator->SetOffsetAdd( 0.01 );
  applyKmeansEstimator->SetOffsetMultiply( 0.01 );
  applyKmeansEstimator->SetMaxSplitAttempts( 10 );
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
      error += itk::Math::abs(errorForClass[i]/referenceCodebookForClass[i]);
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
  std::cout << "KMeansResults\n" << kmeansResult << std::endl;

  applyKmeansEstimator->SetCodebook(inCDBK);
  applyKmeansEstimator->Update();
  applyKmeansEstimator->Print(std::cout);

  membershipFunctions = applyKmeansEstimator->GetMembershipFunctions();

  //Testing for the various parameter access functions in the test
  std::cout << "The final codebook (cluster centers are: " << std::endl;
  std::cout << applyKmeansEstimator->GetCodebook() << std::endl;
  std::cout << "The threshold parameter used was: " <<
    applyKmeansEstimator->GetThreshold() << std::endl;
  std::cout << "The additive ofset parameter used was: " <<
    applyKmeansEstimator->GetOffsetAdd() << std::endl;
  std::cout << "The multiplicative ofset parameter used was: " <<
    applyKmeansEstimator->GetOffsetMultiply() << std::endl;
  std::cout << "The maximum number of attempted splits in codebook: " <<
    applyKmeansEstimator->GetMaxSplitAttempts() << std::endl;
  std::cout << "  " << std::endl;

  //Testing the distance of the first pixel to the centroids; identify the class
  //closest to the fist pixel.
  unsigned int minidx = 0;
  double mindist = 99999999;
  double classdist;
  for( unsigned int idx=0; idx < membershipFunctions.size(); idx++ )
    {
    classdist = membershipFunctions[idx]->Evaluate( outIt.Get() );
    std::cout << "Distance of first pixel to class " << idx << " is: " << classdist << std::endl;
    if( mindist > classdist  )
      {
      mindist = classdist;
      minidx = idx;
      }
    }

  //Validation with initial Kmeans estimate provided as input by the user
  error =0;
  meanCDBKvalue = 0;
  const size_t test = membershipFunctions.size();
  for(unsigned int classIndex=0; classIndex < test; classIndex++ )
    {
    kmeansResultForClass = membershipFunctions[classIndex]->GetCentroid();
    referenceCodebookForClass = inCDBK.get_row( classIndex);
    errorForClass = kmeansResultForClass - referenceCodebookForClass;

    for(int i = 0; i < NUMBANDS; i++)
      {
      error += itk::Math::abs(errorForClass[i]/referenceCodebookForClass[i]);
      meanCDBKvalue += referenceCodebookForClass[i];
      }
    }

  error /= NCODEWORDS*NUMBANDS;
  meanCDBKvalue /= NCODEWORDS*NUMBANDS;

  //Check if the mean codebook is within error limits and the first pixel
  //is labeled to belong to class 2
  if( (error < 0.1 * meanCDBKvalue) && (minidx == 2) )
    {
    std::cout << "Kmeans algorithm passed (with initial input)"<<std::endl;
    }
  else
    {
    std::cout << "Kmeans algorithm failed (with initial input)"<<std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
