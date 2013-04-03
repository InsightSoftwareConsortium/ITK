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

#include "itkIterativeSupervisedTrainingFunction.h"
#include "itkRBFNetwork.h"
#include "itkListSample.h"
#include <fstream>

#include "itkWeightedCentroidKdTreeGenerator.h"
#include "itkKdTreeBasedKmeansEstimator.h"
#include "itkRBFBackPropagationLearningFunction.h"

#define ROUND(x) (floor(x+0.5))

  int
RBFTest1(int argc, char* argv[])
{
  if (argc < 3)
    {
    std::cout << "Usage: " << argv[0]
              << " InputTrainingFile(.txt) InputTestFile(.txt)" << std::endl;
    return EXIT_FAILURE;
    }

  int num_input_nodes = 3;
  int num_hidden_nodes = 2;  // 2 2 radial basis functions
  int num_output_nodes = 2;

  typedef itk::Array<double> MeasurementVectorType;
  typedef itk::Array<double> TargetVectorType;

  typedef itk::Statistics::ListSample<MeasurementVectorType> SampleType;
  typedef itk::Statistics::ListSample<TargetVectorType>      TargetType;

  typedef itk::Statistics::EuclideanDistanceMetric<MeasurementVectorType> DistanceMetricType;

  int num_train=1000;
  int num_test=200;

  MeasurementVectorType mv(num_input_nodes);
  TargetVectorType tv(num_output_nodes);
  TargetVectorType ov(num_output_nodes);
  SampleType::Pointer trainsample = SampleType::New();
  SampleType::Pointer testsample = SampleType::New();
  TargetType::Pointer traintargets = TargetType::New();
  TargetType::Pointer testtargets = TargetType::New();
  trainsample->SetMeasurementVectorSize( num_input_nodes);
  traintargets->SetMeasurementVectorSize( num_output_nodes);
  testsample->SetMeasurementVectorSize( num_input_nodes);
  testtargets->SetMeasurementVectorSize( num_output_nodes);

  char* trainFileName = argv[1];
  char* testFileName = argv[2];

  std::ifstream infile1;
  infile1.open(trainFileName, std::ios::in);
  if (infile1.fail())
    {
    std::cout << argv[0] << " Cannot open training file for reading: "
              << trainFileName << std::endl;
    return EXIT_FAILURE;
    }

  for (int a = 0; a < num_train; a++)
    {
    for (int i = 0; i < num_input_nodes; i++)
      {
      infile1 >> mv[i];
      }
    for (int i = 0; i < num_output_nodes; i++)
      {
      infile1 >> tv[i];
      }
    trainsample->PushBack(mv);
    traintargets->PushBack(tv);
    }
  infile1.close();
  std::ifstream infile2;
  infile2.open(testFileName, std::ios::in);
  if (infile2.fail())
    {
    std::cout << argv[0] << " Cannot open test file for reading: "
              << testFileName << std::endl;
    return EXIT_FAILURE;
    }

  for (int a = 0; a < num_test; a++)
    {
    for (int i = 0; i < num_input_nodes; i++)
      {
      infile2 >> mv[i];
      }
    for (int i = 0; i < num_output_nodes; i++)
      {
      infile2 >> tv[i];
      }
    testsample->PushBack(mv);
    testtargets->PushBack(tv);
    }
  infile2.close();

  typedef itk::Statistics::RBFNetwork<MeasurementVectorType, TargetVectorType>
    RBFNetworkType;
  std::cout<<trainsample->Size()<<std::endl;
  RBFNetworkType::Pointer net1 = RBFNetworkType::New();
  net1->SetNumOfInputNodes(num_input_nodes);
  net1->SetNumOfFirstHiddenNodes(num_hidden_nodes);
  net1->SetNumOfOutputNodes(num_output_nodes);
  net1->SetFirstHiddenLayerBias(1.0);
  net1->SetOutputLayerBias(1.0);
  net1->SetClasses(2);

  typedef itk::Statistics::RBFBackPropagationLearningFunction<
    RBFNetworkType::LayerInterfaceType, TargetVectorType> RBFLearningFunctionType;
  RBFLearningFunctionType::Pointer learningfunction=RBFLearningFunctionType::New();

  net1->SetLearningFunction(learningfunction.GetPointer());


  //Kmeans Initialization of RBF Centers
  typedef itk::Statistics::WeightedCentroidKdTreeGenerator< SampleType >
    TreeGeneratorType;
  TreeGeneratorType::Pointer treeGenerator = TreeGeneratorType::New();

  treeGenerator->SetSample( trainsample );
  treeGenerator->SetBucketSize( 16 );
  treeGenerator->Update();

  typedef TreeGeneratorType::KdTreeType                         TreeType;
  typedef itk::Statistics::KdTreeBasedKmeansEstimator<TreeType> EstimatorType;
  EstimatorType::Pointer estimator = EstimatorType::New();

  int m1 = rand() % num_train;
  int m2 = rand() % num_train;
  MeasurementVectorType c1 = trainsample->GetMeasurementVector(m1);
  MeasurementVectorType c2 =  trainsample->GetMeasurementVector(m2);

  EstimatorType::ParametersType initialMeans(6);
  for(int i=0; i<3; i++)
    {
    initialMeans[i] = c1[i];
    }
  for(int i=3; i<6; i++)
    {
    initialMeans[i] = c2[i-3];
    }
  std::cout << c1 << " " << c2 <<std::endl;

  estimator->SetParameters( initialMeans );
  estimator->SetKdTree( treeGenerator->GetOutput() );
  estimator->SetMaximumIteration( 200 );
  estimator->SetCentroidPositionChangesThreshold(0.0);

  estimator->StartOptimization();

  EstimatorType::ParametersType estimatedMeans = estimator->GetParameters();
  std::cout << estimatedMeans.size() << std::endl;
  std::cout << estimatedMeans << std::endl;

  MeasurementVectorType initialcenter1(num_input_nodes);
  initialcenter1[0]=estimatedMeans[0]; //110;
  initialcenter1[1]=estimatedMeans[1]; //250;
  initialcenter1[2]=estimatedMeans[2]; //50;
  net1->SetCenter(initialcenter1);

  MeasurementVectorType initialcenter2(num_input_nodes);
  initialcenter2[0]=estimatedMeans[3]; //99;
  initialcenter2[1]=estimatedMeans[4]; //199;
  initialcenter2[2]=estimatedMeans[5]; //300;
  net1->SetCenter(initialcenter2);

  DistanceMetricType::Pointer DistanceMetric = DistanceMetricType::New();
  double width = DistanceMetric->Evaluate(initialcenter1,initialcenter2);

  net1->SetRadius(2*width);
  net1->SetRadius(2*width);

  net1->Initialize();
  net1->InitializeWeights();
  net1->SetLearningRate(0.5);

  typedef itk::Statistics::IterativeSupervisedTrainingFunction<SampleType, TargetType, double> TrainingFcnType;

  TrainingFcnType::Pointer trainingfcn = TrainingFcnType::New();
  trainingfcn->SetIterations(500);
  trainingfcn->SetThreshold(0.001);
  trainingfcn->Train(net1, trainsample, traintargets);

  //Network Simulation
  std::cout << testsample->Size() << std::endl;
  std::cout << "Network Simulation" << std::endl;
  SampleType::ConstIterator iter1 = testsample->Begin();
  TargetType::ConstIterator iter2 = testtargets->Begin();
  unsigned int error1 = 0;
  unsigned int error2 = 0;
  int flag;
  int class_id;
  std::ofstream outfile;
  outfile.open("out1.txt",std::ios::out);
  int count =0;
  while (iter1 != testsample->End())
    {
    mv = iter1.GetMeasurementVector();
    tv = iter2.GetMeasurementVector();
    ov = net1->GenerateOutput(mv);
    std::cout << "Target = " << tv << std::endl;
    std::cout << "Output = " << ov << std::endl;
    flag=0;
    if(ov[0]>ov[1])
      {
      class_id=1;
      }
    else
      {
      class_id=-1;
      }
    if(class_id==1 && count >100)
      {
      flag =1;
      }
    if(class_id==-1 && count <100)
      {
      flag =2;
      }

    if (flag == 1)
      {
      ++error1;
      }
    else if (flag == 2)
      {
      ++error2;
      }
    outfile << mv << " " << tv << " " << ov << std::endl;
    std::cout << "Network Input = " << mv << std::endl;
    std::cout << "Network Output = " << ov << std::endl;
    std::cout << "Target = " << tv << std::endl;
    ++iter1;
    ++iter2;
    count++;
    }
  std::cout << "Among "<<num_test<<" measurement vectors, " << error1 + error2
    << " vectors are misclassified." << std::endl;
  std::cout << "Network Weights = " << std::endl;
  std::cout << net1 << std::endl;
  std::cout << error1 << " " << error2 <<std::endl;
  std::cout << "Test passed." << std::endl;

  if (double(error1 / 10) > 5 || double(error2 / 10) > 5)
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }


  if (double(error1 / 10) > 2 || double(error2 / 10) > 2)
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
