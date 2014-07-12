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

#include "itkImageGaussianModelEstimator.h"
#include "itkMahalanobisDistanceMembershipFunction.h"
#include "itkMinimumDecisionRule.h"
#include "itkImageClassifierBase.h"


// This tests the supervised image classifier methods. The test,
// however, only exercises a pathalogical case, where the covariances
// of all the classes are singular.  In this case, the methods degrade
// to classifying based on euclidean distance to the mean.


//Data definitons

namespace SupervisedImageClassifierTest
{

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

}


int itkSupervisedImageClassifierTest(int, char* [] )
{
  const unsigned int IMGWIDTH       =    2;
  const unsigned int IMGHEIGHT      =    2;
  const unsigned int NFRAMES        =    4;
  const unsigned int NUMBANDS       =    2;
  const unsigned int NDIMENSION     =    3;
  const unsigned int NUM_CLASSES    =    3;

  //------------------------------------------------------
  //Create a simple test image with width, height, and
  //depth 4 vectors each with each vector having data for
  //2 bands.
  //------------------------------------------------------
  typedef itk::Image<itk::Vector<double,NUMBANDS>,NDIMENSION> VecImageType;

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
  typedef VecImageType::PixelType VecImagePixelType;

  enum { VecImageDimension = VecImageType::ImageDimension };
  typedef
    itk::ImageRegionIterator< VecImageType > VecIterator;

  VecIterator outIt( vecImage, vecImage->GetBufferedRegion() );

  //--------------------------------------------------------------------------
  //Manually create and store each vector
  //--------------------------------------------------------------------------

  //Slice 1
  //Vector no. 1
  VecImagePixelType vec;
  vec.Fill(21); outIt.Set( vec ); ++outIt;
  //Vector no. 2
  vec.Fill(20); outIt.Set( vec ); ++outIt;
  //Vector no. 3
  vec.Fill(8); outIt.Set( vec ); ++outIt;
  //Vector no. 4
  vec.Fill(10); outIt.Set( vec ); ++outIt;
  //Slice 2
  //Vector no. 1
  vec.Fill(22); outIt.Set( vec ); ++outIt;
  //Vector no. 2
  vec.Fill(21); outIt.Set( vec ); ++outIt;
  //Vector no. 3
  vec.Fill(11); outIt.Set( vec ); ++outIt;
  //Vector no. 4
  vec.Fill(9); outIt.Set( vec ); ++outIt;

  //Slice 3
  //Vector no. 1
  vec.Fill(19); outIt.Set( vec ); ++outIt;
  //Vector no. 2
  vec.Fill(19); outIt.Set( vec ); ++outIt;
  //Vector no. 3
  vec.Fill(11); outIt.Set( vec ); ++outIt;
  //Vector no. 4
  vec.Fill(11); outIt.Set( vec ); ++outIt;

  //Slice 4
  //Vector no. 1
  vec.Fill(18); outIt.Set( vec ); ++outIt;
  //Vector no. 2
  vec.Fill(18); outIt.Set( vec ); ++outIt;
  //Vector no. 3
  vec.Fill(12); outIt.Set( vec ); ++outIt;
  //Vector no. 4
  vec.Fill(14); outIt.Set( vec ); ++outIt;

  //---------------------------------------------------------------
  //Generate the training data
  //---------------------------------------------------------------
  typedef itk::Image<unsigned short,NDIMENSION> ClassImageType;
  ClassImageType::Pointer classImage  = ClassImageType::New();

  ClassImageType::SizeType classImgSize = {{ IMGWIDTH , IMGHEIGHT, NFRAMES }};

  ClassImageType::IndexType classindex;
  classindex.Fill(0);

  ClassImageType::RegionType classregion;

  classregion.SetSize( classImgSize );
  classregion.SetIndex( classindex );

  classImage->SetLargestPossibleRegion( classregion );
  classImage->SetBufferedRegion( classregion );
  classImage->Allocate();

  // setup the iterators
  typedef ClassImageType::PixelType ClassImagePixelType;

  typedef
    itk::ImageRegionIterator<ClassImageType> ClassImageIterator;

  ClassImageIterator
    classoutIt( classImage, classImage->GetBufferedRegion() );

  ClassImagePixelType outputPixel;
  //--------------------------------------------------------------------------
  //Manually create and store each vector
  //--------------------------------------------------------------------------
  //Slice 1
  //Pixel no. 1
  outputPixel = 2;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 2
  outputPixel = 2;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 3
  outputPixel = 1;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 4
  outputPixel = 1;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Slice 2
  //Pixel no. 1
  outputPixel = 0;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 2
  outputPixel = 0;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 3
  outputPixel = 0;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 4
  outputPixel = 0;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Slice 3
  //Pixel no. 1
  outputPixel = 2;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 2
  outputPixel = 2;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 3
  outputPixel = 1;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 4
  outputPixel = 1;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Slice 4
  //Pixel no. 1
  outputPixel = 0;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 2
  outputPixel = 0;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 3
  outputPixel = 0;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 4
  outputPixel = 0;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //----------------------------------------------------------------------
  //Set membership function (Using the statistics objects)
  //----------------------------------------------------------------------
  namespace stat = itk::Statistics;

  typedef stat::MahalanobisDistanceMembershipFunction< VecImagePixelType >
    MembershipFunctionType;
  typedef MembershipFunctionType::Pointer
    MembershipFunctionPointer;
  typedef std::vector< MembershipFunctionPointer >
    MembershipFunctionPointerVector;

  //----------------------------------------------------------------------
  //Set the image model estimator
  //----------------------------------------------------------------------
  typedef itk::ImageGaussianModelEstimator<VecImageType,
    MembershipFunctionType, ClassImageType>
    ImageGaussianModelEstimatorType;

  ImageGaussianModelEstimatorType::Pointer
    applyEstimateModel = ImageGaussianModelEstimatorType::New();

  applyEstimateModel->SetNumberOfModels(NUM_CLASSES);
  applyEstimateModel->SetInputImage(vecImage);
  applyEstimateModel->SetTrainingImage(classImage);

  //Run the gaussian classifier algorithm
  applyEstimateModel->Update();
  applyEstimateModel->Print(std::cout);

  MembershipFunctionPointerVector membershipFunctions =
    applyEstimateModel->GetMembershipFunctions();

  //----------------------------------------------------------------------
  //Set the decision rule
  //----------------------------------------------------------------------
  typedef itk::Statistics::DecisionRule::Pointer DecisionRuleBasePointer;

  typedef itk::Statistics::MinimumDecisionRule DecisionRuleType;
  DecisionRuleType::Pointer
    myDecisionRule = DecisionRuleType::New();

  //----------------------------------------------------------------------
  // Test code for the supervised classifier algorithm
  //----------------------------------------------------------------------

  //---------------------------------------------------------------------
  // Multiband data is now available in the right format
  //---------------------------------------------------------------------

  typedef itk::ImageClassifierBase< VecImageType,
    ClassImageType > SupervisedClassifierType;

  SupervisedClassifierType::Pointer
    applyClassifier = SupervisedClassifierType::New();

  typedef SupervisedImageClassifierTest::ShowProgressObject ProgressType;

  ProgressType progressWatch(applyClassifier);
  itk::SimpleMemberCommand<ProgressType>::Pointer command;
  command = itk::SimpleMemberCommand<ProgressType>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ProgressType::ShowProgress);
  applyClassifier->AddObserver(itk::ProgressEvent(), command);

  // Set the Classifier parameters
  applyClassifier->SetNumberOfClasses(NUM_CLASSES);
  applyClassifier->SetInputImage(vecImage);

  // Set the decision rule
  applyClassifier->
    SetDecisionRule((DecisionRuleBasePointer) myDecisionRule );

  //Add the membership functions
  for( unsigned int i=0; i<NUM_CLASSES; i++ )
    {
    applyClassifier->AddMembershipFunction( membershipFunctions[i] );
    }

  //Run the gaussian classifier algorithm
  applyClassifier->Update();

  //Get the classified image
  ClassImageType::Pointer
    outClassImage = applyClassifier->GetClassifiedImage();

  applyClassifier->Print(std::cout);

  //Print the gaussian classified image
  ClassImageIterator labeloutIt( outClassImage,
                                 outClassImage->GetBufferedRegion() );

  int i=0;
  while(!labeloutIt.IsAtEnd())
    {
    //Print the classified index
    int classIndex = (int) labeloutIt.Get();
    std::cout << " Pixel No " << i << " Value " << classIndex << std::endl;
    ++i;
    ++labeloutIt;
    }//end while

  //Verify if the results were as per expectation
  labeloutIt.GoToBegin();
  bool passTest = true;

  //Loop through the data set
  while(!labeloutIt.IsAtEnd())
    {
    int classIndex = (int) labeloutIt.Get();
    if (classIndex != 2)
      {
      passTest = false;
      break;
      }
    ++labeloutIt;

    classIndex = (int) labeloutIt.Get();
    if (classIndex != 2)
      {
      passTest = false;
      break;
      }
    ++labeloutIt;

    classIndex = (int) labeloutIt.Get();
    if (classIndex != 1)
      {
      passTest = false;
      break;
      }
    ++labeloutIt;

    classIndex = (int) labeloutIt.Get();
    if (classIndex != 1)
      {
      passTest = false;
      break;
      }
    ++labeloutIt;

    }//end while

  if( passTest == true )
    {
    std::cout<< "Supervised Classifier Test Passed" << std::endl;
    }
  else
    {
    std::cout<< "Supervised Classifier Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
