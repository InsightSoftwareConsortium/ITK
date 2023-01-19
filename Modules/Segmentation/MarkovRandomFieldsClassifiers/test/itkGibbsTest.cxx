/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include <iostream>


#include "itkRGBGibbsPriorFilter.h"

#include "itkImageGaussianModelEstimator.h"
#include "itkMahalanobisDistanceMembershipFunction.h"
#include "itkMinimumDecisionRule.h"
#include "itkTestingMacros.h"


int
itkGibbsTest(int, char *[])
{

  constexpr unsigned int ImageWidth = 20;
  constexpr unsigned int ImageHeight = 20;
  constexpr unsigned int NumFrames = 1;
  constexpr unsigned int NumberOfBands = 1;
  constexpr unsigned int ImageDimension = 3;
  constexpr unsigned int NumClasses = 3;
  constexpr unsigned int MaxNumIter = 1;

  const unsigned short TestingImage[400] = {
    297, 277, 317, 289, 300, 312, 306, 283, 282, 308, 308, 342, 335, 325, 315, 300, 304, 318, 307, 308,

    319, 276, 311, 282, 309, 273, 308, 277, 296, 313, 308, 333, 322, 317, 302, 330, 339, 340, 325, 315,

    272, 316, 296, 299, 298, 310, 271, 319, 315, 280, 338, 342, 349, 349, 330, 319, 313, 314, 342, 301,

    274, 274, 312, 282, 277, 303, 313, 300, 275, 292, 341, 336, 324, 310, 337, 323, 322, 347, 337, 305,

    296, 272, 304, 304, 281, 304, 302, 284, 315, 270, 325, 349, 337, 317, 308, 332, 324, 303, 334, 325,

    291, 272, 289, 317, 289, 310, 305, 316, 292, 307, 307, 343, 341, 329, 309, 308, 340, 323, 307, 325,

    274, 286, 282, 291, 270, 296, 274, 288, 274, 275, 341, 301, 325, 333, 321, 305, 347, 346, 327, 317,

    282, 315, 270, 314, 290, 304, 297, 304, 309, 290, 309, 338, 341, 319, 325, 344, 301, 349, 328, 302,

    314, 289, 296, 270, 274, 277, 317, 280, 278, 285, 315, 347, 314, 316, 307, 336, 341, 335, 330, 337,

    281, 291, 317, 317, 302, 304, 272, 277, 318, 319, 305, 322, 337, 334, 327, 303, 321, 310, 334, 314,

    321, 311, 328, 326, 331, 308, 325, 348, 334, 346, 309, 316, 308, 349, 322, 349, 304, 331, 304, 321,

    346, 302, 344, 314, 311, 338, 320, 310, 331, 330, 322, 323, 329, 331, 342, 341, 331, 336, 328, 318,

    309, 336, 327, 345, 312, 309, 330, 334, 329, 317, 324, 304, 337, 330, 331, 334, 340, 307, 328, 343,

    345, 330, 336, 302, 333, 348, 315, 328, 315, 308, 305, 343, 342, 337, 307, 316, 303, 303, 332, 341,

    327, 322, 320, 314, 323, 325, 307, 316, 336, 315, 341, 347, 343, 336, 315, 347, 306, 303, 339, 326,

    330, 347, 303, 343, 332, 316, 305, 325, 311, 314, 345, 327, 333, 305, 324, 318, 324, 339, 325, 319,

    334, 326, 330, 319, 300, 335, 305, 331, 343, 324, 337, 324, 319, 339, 327, 317, 347, 331, 308, 318,

    306, 337, 347, 330, 301, 316, 302, 331, 306, 342, 343, 329, 336, 342, 300, 306, 335, 330, 310, 303,

    308, 331, 317, 315, 318, 333, 340, 340, 326, 330, 339, 345, 307, 331, 320, 312, 306, 342, 303, 321,

    328, 315, 327, 311, 315, 305, 340, 306, 314, 339, 344, 339, 337, 330, 318, 342, 311, 343, 311, 312
  };

  using PixelType = itk::Vector<unsigned short, NumberOfBands>;
  using VecImageType = itk::Image<PixelType, ImageDimension>;

  auto vecImage = VecImageType::New();

  using VecImagePixelType = VecImageType::PixelType;

  VecImageType::SizeType vecImgSize = { { ImageWidth, ImageHeight, NumFrames } };

  VecImageType::IndexType index;
  index.Fill(0);

  VecImageType::RegionType region;

  region.SetSize(vecImgSize);
  region.SetIndex(index);

  vecImage->SetLargestPossibleRegion(region);
  vecImage->SetBufferedRegion(region);
  vecImage->Allocate();

  enum
  {
    VecImageDimension = VecImageType::ImageDimension
  };
  using VecIterator = itk::ImageRegionIterator<VecImageType>;

  VecIterator outIt(vecImage, vecImage->GetBufferedRegion());
  outIt.GoToBegin();

  // Set up the vector to store the image  data
  using DataVector = VecImageType::PixelType;
  DataVector dblVec;

  //--------------------------------------------------------------------------
  // Manually create and store each vector
  //--------------------------------------------------------------------------
  // Slice 1
  // Vector 1
  int i = 0;
  while (!outIt.IsAtEnd())
  {
    dblVec[0] = static_cast<unsigned short>(TestingImage[i]);
    //  dblVec[1] = (unsigned short) TestImage[i+65536];
    //  dblVec[2] = (unsigned short) TestImage[i+65536*2];
    outIt.Set(dblVec);
    ++outIt;
    i++;
  }

  //---------------------------------------------------------------
  // Generate the training data
  //---------------------------------------------------------------
  using ClassImageType = itk::Image<unsigned short, ImageDimension>;
  auto classImage = ClassImageType::New();

  ClassImageType::SizeType classImgSize = { { ImageWidth, ImageHeight, NumFrames } };

  ClassImageType::IndexType classindex;
  classindex.Fill(0);

  ClassImageType::RegionType classregion;

  classregion.SetSize(classImgSize);
  classregion.SetIndex(classindex);

  classImage->SetLargestPossibleRegion(classregion);
  classImage->SetBufferedRegion(classregion);
  classImage->Allocate();

  using ClassImageIterator = itk::ImageRegionIterator<ClassImageType>;

  ClassImageIterator classoutIt(classImage, classImage->GetBufferedRegion());
  classoutIt.GoToBegin();

  //--------------------------------------------------------------------------
  // Manually create and store each vector
  //--------------------------------------------------------------------------
  // Slice 1
  // Pixel no. 1

  i = 0;
  while (!classoutIt.IsAtEnd())
  {
    if ((i % ImageWidth < 8) && (i % ImageWidth > 4) && (i / ImageWidth < 8) && (i / ImageWidth > 4))
    {
      classoutIt.Set(1);
    }
    else
    {
      if ((i % ImageWidth < 18) && (i % ImageWidth > 14) && (i / ImageWidth < 18) && (i / ImageWidth > 14))
      {
        classoutIt.Set(2);
      }
      else
      {
        classoutIt.Set(0);
      }
    }
    ++classoutIt;
    i++;
  }

  //----------------------------------------------------------------------
  // Test code for the supervised classifier algorithm
  //----------------------------------------------------------------------

  //---------------------------------------------------------------------
  // Multiband data is now available in the right format
  //---------------------------------------------------------------------

  //----------------------------------------------------------------------
  // Set membership function (Using the statistics objects)
  //----------------------------------------------------------------------

  namespace stat = itk::Statistics;

  using MembershipFunctionType = stat::MahalanobisDistanceMembershipFunction<VecImagePixelType>;
  using MembershipFunctionPointer = MembershipFunctionType::Pointer;

  using MembershipFunctionPointerVector = std::vector<MembershipFunctionPointer>;

  //----------------------------------------------------------------------
  // Set the image model estimator (train the class models)
  //----------------------------------------------------------------------

  using ImageGaussianModelEstimatorType =
    itk::ImageGaussianModelEstimator<VecImageType, MembershipFunctionType, ClassImageType>;

  auto applyEstimateModel = ImageGaussianModelEstimatorType::New();

  applyEstimateModel->SetNumberOfModels(NumClasses);
  applyEstimateModel->SetInputImage(vecImage);
  applyEstimateModel->SetTrainingImage(classImage);

  // Run the gaussian classifier algorithm
  applyEstimateModel->Update();
  applyEstimateModel->Print(std::cout);

  MembershipFunctionPointerVector membershipFunctions = applyEstimateModel->GetMembershipFunctions();

  //----------------------------------------------------------------------
  // Set the decision rule
  //----------------------------------------------------------------------
  using DecisionRuleBasePointer = itk::Statistics::DecisionRule::Pointer;

  using DecisionRuleType = itk::Statistics::MinimumDecisionRule;
  auto myDecisionRule = DecisionRuleType::New();

  //----------------------------------------------------------------------
  // Set the classifier to be used and assigne the parameters for the
  // supervised classifier algorithm except the input image which is
  // grabbed from the MRF application pipeline.
  //----------------------------------------------------------------------
  //---------------------------------------------------------------------
  using ClassifierType = itk::ImageClassifierBase<VecImageType, ClassImageType>;

  using ClassifierPointer = ClassifierType::Pointer;
  ClassifierPointer myClassifier = ClassifierType::New();
  // Set the Classifier parameters
  myClassifier->SetNumberOfClasses(NumClasses);

  // Set the decision rule
  myClassifier->SetDecisionRule((DecisionRuleBasePointer)myDecisionRule);

  // Add the membership functions
  for (unsigned int ii = 0; ii < NumClasses; ++ii)
  {
    myClassifier->AddMembershipFunction(membershipFunctions[ii]);
  }

  // Set the Gibbs Prior labeller
  using GibbsPriorFilterType = itk::RGBGibbsPriorFilter<VecImageType, ClassImageType>;
  auto applyGibbsImageFilter = GibbsPriorFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(applyGibbsImageFilter, RGBGibbsPriorFilter, MRFImageFilter);

  // Set the MRF labeller parameters
  applyGibbsImageFilter->SetNumberOfClasses(NumClasses);
  ITK_TEST_SET_GET_VALUE(NumClasses, applyGibbsImageFilter->GetNumberOfClasses());

  applyGibbsImageFilter->SetMaximumNumberOfIterations(MaxNumIter);
  ITK_TEST_SET_GET_VALUE(MaxNumIter, applyGibbsImageFilter->GetMaximumNumberOfIterations());

  // Set the MRF labeller parameters
  applyGibbsImageFilter->SetNumberOfClasses(NumClasses);
  applyGibbsImageFilter->SetMaximumNumberOfIterations(MaxNumIter);

  //  applyGibbsImageFilter->SetErrorTollerance(0.00);
  applyGibbsImageFilter->SetClusterSize(10);
  applyGibbsImageFilter->SetBoundaryGradient(6);
  applyGibbsImageFilter->SetObjectLabel(1);
  //  applyGibbsImageFilter->SetRecursiveNumber(1);

  double cliqueWeight1 = 5.0;
  applyGibbsImageFilter->SetCliqueWeight_1(cliqueWeight1);
  ITK_TEST_SET_GET_VALUE(cliqueWeight1, applyGibbsImageFilter->GetCliqueWeight_1());

  double cliqueWeight2 = 5.0;
  applyGibbsImageFilter->SetCliqueWeight_2(cliqueWeight2);
  ITK_TEST_SET_GET_VALUE(cliqueWeight2, applyGibbsImageFilter->GetCliqueWeight_2());

  double cliqueWeight3 = 5.0;
  applyGibbsImageFilter->SetCliqueWeight_3(cliqueWeight3);
  ITK_TEST_SET_GET_VALUE(cliqueWeight3, applyGibbsImageFilter->GetCliqueWeight_3());

  double cliqueWeight4 = 5.0;
  applyGibbsImageFilter->SetCliqueWeight_4(cliqueWeight4);
  ITK_TEST_SET_GET_VALUE(cliqueWeight4, applyGibbsImageFilter->GetCliqueWeight_4());

  double cliqueWeight5 = 5.0;
  applyGibbsImageFilter->SetCliqueWeight_5(cliqueWeight5);
  ITK_TEST_SET_GET_VALUE(cliqueWeight5, applyGibbsImageFilter->GetCliqueWeight_5());

  double cliqueWeight6 = 0.0;
  applyGibbsImageFilter->SetCliqueWeight_6(cliqueWeight6);
  ITK_TEST_SET_GET_VALUE(cliqueWeight6, applyGibbsImageFilter->GetCliqueWeight_6());

  applyGibbsImageFilter->SetInput(vecImage);
  applyGibbsImageFilter->SetClassifier(myClassifier);

  applyGibbsImageFilter->SetObjectThreshold(5.0);

  // Since a suvervised classifier is used, it requires a training image
  applyGibbsImageFilter->SetTrainingImage(classImage);

  // Kick off the Gibbs labeller function
  applyGibbsImageFilter->Update();

  ClassImageType::Pointer outClassImage = applyGibbsImageFilter->GetOutput();

  // Print the mrf labelled image
  ClassImageIterator labeloutIt(outClassImage, outClassImage->GetBufferedRegion());

  int j0 = 0;
  int j1 = 0;
  while (!labeloutIt.IsAtEnd())
  {
    if (labeloutIt.Get() == 0)
    {
      j0++;
    }
    if (labeloutIt.Get() == 1)
    {
      j1++;
    }
    ++labeloutIt;
  }

  std::cout << "j0:" << j0 << std::endl;
  std::cout << "j1:" << j1 << std::endl;

  //  FILE *output=fopen("new.raw", "wb");
  //  fwrite(outImage, 2, ImageWidth*ImageHeight, output);
  //  fclose(output);
  // Verify if the results were as per expectation

  bool passTest;
  /*  int j = 0;
    i = 0;
    labeloutIt.GoToBegin();
    while ( !labeloutIt.IsAtEnd() ) {
    if ((i%ImageWidth<10) && (i/ImageWidth<10) && (labeloutIt.Get()==1))
    {
      j++;
    }
    i++;
    ++labeloutIt;
    }
  */
  passTest = ((j1 > 285) && (j1 < 315));
  if (passTest)
  {
    std::cout << "Gibbs Prior Test Passed" << std::endl;
  }
  else
  {
    std::cout << "Gibbs Prior Test failed" << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
