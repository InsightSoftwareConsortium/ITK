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

// Software Guide : BeginLatex
//
// This example illustrates the use of the \doxygen{RGBGibbsPriorFilter}.
// The filter outputs a binary segmentation that can be improved by the
// deformable model. It is the first part of our hybrid framework.
//
// First, we include the appropriate header file.
//
// Software Guide : EndLatex

#include <iostream>
#include <string>
#include <cmath>

// Software Guide : BeginCodeSnippet
#include "itkRGBGibbsPriorFilter.h"
// Software Guide : EndCodeSnippet

// classes help the Gibbs filter to segment the image
#include "itkImageClassifierBase.h"
#include "itkImageGaussianModelEstimator.h"
#include "itkMahalanobisDistanceMembershipFunction.h"
#include "itkMinimumDecisionRule.h"

// image storage and I/O classes
#include "itkSize.h"
#include "itkImage.h"
#include "itkVector.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#define NUM_CLASSES 3
#define MAX_NUM_ITER 1

int
main(int argc, char * argv[])
{
  if (argc != 4)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage trainimage outputImage" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Gibbs Prior Test Begins: " << std::endl;

  //  Software Guide : BeginLatex
  //
  //  The input is a single channel 2D image; the channel number is
  //  \code{NUMBANDS} = 1, and \code{NDIMENSION} is set to 3.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  constexpr unsigned short NUMBANDS = 1;
  constexpr unsigned short NDIMENSION = 3;

  using VecImageType =
    itk::Image<itk::Vector<unsigned short, NUMBANDS>, NDIMENSION>;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The Gibbs prior segmentation is performed first to generate a rough
  //  segmentation that yields a sample of tissue from a region to be
  //  segmented, which will be combined to form the input for the
  //  isocontouring method. We define the pixel type of the output of the
  //  Gibbs prior filter to be \code{unsigned short}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using ClassImageType = itk::Image<unsigned short, NDIMENSION>;
  // Software Guide : EndCodeSnippet


  // We instantiate reader and writer types
  //
  using ReaderType = itk::ImageFileReader<ClassImageType>;
  using WriterType = itk::ImageFileWriter<ClassImageType>;

  auto inputimagereader = ReaderType::New();
  auto trainingimagereader = ReaderType::New();
  auto writer = WriterType::New();

  inputimagereader->SetFileName(argv[1]);
  trainingimagereader->SetFileName(argv[2]);
  writer->SetFileName(argv[3]);


  // We convert the input into vector images
  //
  auto vecImage = VecImageType::New();
  using VecImagePixelType = VecImageType::PixelType;
  VecImageType::SizeType vecImgSize = { { 181, 217, 1 } };

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

  VecIterator vecIt(vecImage, vecImage->GetBufferedRegion());
  vecIt.GoToBegin();

  inputimagereader->Update();
  trainingimagereader->Update();

  using ClassIterator = itk::ImageRegionIterator<ClassImageType>;

  ClassIterator inputIt(inputimagereader->GetOutput(),
                        inputimagereader->GetOutput()->GetBufferedRegion());
  inputIt.GoToBegin();

  // Set up the vector to store the image  data
  using DataVector = VecImageType::PixelType;
  DataVector dblVec;

  while (!vecIt.IsAtEnd())
  {
    dblVec[0] = inputIt.Get();
    vecIt.Set(dblVec);
    ++vecIt;
    ++inputIt;
  }

  //----------------------------------------------------------------------
  // Set membership function (Using the statistics objects)
  //----------------------------------------------------------------------

  namespace stat = itk::Statistics;

  using VecImagePixelType = VecImageType::PixelType;
  using MembershipFunctionType =
    stat::MahalanobisDistanceMembershipFunction<VecImagePixelType>;
  using MembershipFunctionPointer = MembershipFunctionType::Pointer;

  using MembershipFunctionPointerVector =
    std::vector<MembershipFunctionPointer>;

  //----------------------------------------------------------------------
  // Set the image model estimator (train the class models)
  //----------------------------------------------------------------------

  using ImageGaussianModelEstimatorType =
    itk::ImageGaussianModelEstimator<VecImageType,
                                     MembershipFunctionType,
                                     ClassImageType>;

  auto applyEstimateModel = ImageGaussianModelEstimatorType::New();

  applyEstimateModel->SetNumberOfModels(NUM_CLASSES);
  applyEstimateModel->SetInputImage(vecImage);
  applyEstimateModel->SetTrainingImage(trainingimagereader->GetOutput());


  // Run the gaussian classifier algorithm
  applyEstimateModel->Update();

  std::cout << " site 1 " << std::endl;

  applyEstimateModel->Print(std::cout);

  MembershipFunctionPointerVector membershipFunctions =
    applyEstimateModel->GetMembershipFunctions();

  std::cout << " site 2 " << std::endl;

  //----------------------------------------------------------------------
  // Set the decision rule
  //----------------------------------------------------------------------
  using DecisionRuleBasePointer = itk::Statistics::DecisionRule::Pointer;

  using DecisionRuleType = itk::Statistics::MinimumDecisionRule;
  auto myDecisionRule = DecisionRuleType::New();

  std::cout << " site 3 " << std::endl;

  //----------------------------------------------------------------------
  // Set the classifier to be used and assigned the parameters for the
  // supervised classifier algorithm except the input image which is
  // grabbed from the Gibbs application pipeline.
  //----------------------------------------------------------------------
  //---------------------------------------------------------------------

  //  Software Guide : BeginLatex
  //
  //  Then we define the classifier that is needed
  //  for the Gibbs prior model to make correct segmenting decisions.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using ClassifierType =
    itk::ImageClassifierBase<VecImageType, ClassImageType>;
  using ClassifierPointer = ClassifierType::Pointer;
  ClassifierPointer myClassifier = ClassifierType::New();
  // Software Guide : EndCodeSnippet

  // Set the Classifier parameters
  myClassifier->SetNumberOfClasses(NUM_CLASSES);

  // Set the decision rule
  myClassifier->SetDecisionRule((DecisionRuleBasePointer)myDecisionRule);

  // Add the membership functions
  for (unsigned int i = 0; i < NUM_CLASSES; ++i)
  {
    myClassifier->AddMembershipFunction(membershipFunctions[i]);
  }

  // Set the Gibbs Prior labeller
  //  Software Guide : BeginLatex
  //
  //  After that we can define the multi-channel Gibbs prior model.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using GibbsPriorFilterType =
    itk::RGBGibbsPriorFilter<VecImageType, ClassImageType>;
  auto applyGibbsImageFilter = GibbsPriorFilterType::New();
  // Software Guide : EndCodeSnippet

  // Set the MRF labeller parameters
  //  Software Guide : BeginLatex
  //
  //  The parameters for the Gibbs prior filter are defined
  //  below. \code{NumberOfClasses} indicates how many different objects are
  //  in the image.  The maximum number of iterations is the number of
  //  minimization steps.  \code{ClusterSize} sets the lower limit on the
  //  object's size.  The boundary gradient is the estimate of the variance
  //  between objects and background at the boundary region.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  applyGibbsImageFilter->SetNumberOfClasses(NUM_CLASSES);
  applyGibbsImageFilter->SetMaximumNumberOfIterations(MAX_NUM_ITER);
  applyGibbsImageFilter->SetClusterSize(10);
  applyGibbsImageFilter->SetBoundaryGradient(6);
  applyGibbsImageFilter->SetObjectLabel(1);
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  We now set the input classifier for the Gibbs prior filter and the
  //  input to the classifier. The classifier will calculate the mean and
  //  variance of the object using the class image, and the results will be
  //  used as parameters for the Gibbs prior model.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  applyGibbsImageFilter->SetInput(vecImage);
  applyGibbsImageFilter->SetClassifier(myClassifier);
  applyGibbsImageFilter->SetTrainingImage(trainingimagereader->GetOutput());
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Finally we execute the Gibbs prior filter using the Update() method.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  applyGibbsImageFilter->Update();
  // Software Guide : EndCodeSnippet

  std::cout << "applyGibbsImageFilter: " << applyGibbsImageFilter;

  writer->SetInput(applyGibbsImageFilter->GetOutput());
  writer->Update();

  //  Software Guide : BeginLatex
  //
  //  We execute this program on the image \code{brainweb89.png}. The
  //  following parameters are passed to the command line:
  //
  //  \small
  //  \begin{verbatim}
  // GibbsGuide.exe brainweb89.png brainweb89_train.png brainweb_gp.png
  //  \end{verbatim}
  //  \normalsize
  //
  //  \code{brainweb89train} is a training image that helps to estimate the
  //  object statistics.
  //
  //  Note that in order to successfully segment other images, one has to
  //  create suitable training images for them. We can also segment color
  //  (RGB) and other multi-channel images.
  //
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;
}
