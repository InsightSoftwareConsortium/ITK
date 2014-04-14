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
#include "itkOneHiddenLayerBackPropagationNeuralNetwork.h"
#include "itkIterativeSupervisedTrainingFunction.h"
#include "itkListSample.h"
#include <fstream>

#define ROUND(x) (floor(x+0.5))

int
NNetClassifierTest3(int argc, char* argv[])
{
  if (argc < 3)
    {
    std::cout << "Usage: " << argv[0]
              << " InputTrainingFile(.txt) InputTestFile(.txt)" << std::endl;
    return EXIT_FAILURE;
    }

  int num_train=800;
  int num_test=200;

  char* trainFileName =argv[1];
  char* testFileName = argv[2];

  const int num_input_nodes = 2;
  const int num_hidden_nodes = 2;
  const int num_output_nodes = 1;

  typedef itk::Vector<double, num_input_nodes>               MeasurementVectorType;
  typedef itk::Vector<double, num_output_nodes>              TargetVectorType;
  typedef itk::Statistics::ListSample<MeasurementVectorType> SampleType;
  typedef itk::Statistics::ListSample<TargetVectorType>      TargetType;
  typedef itk::Statistics::IterativeSupervisedTrainingFunction<SampleType, TargetType, double>
                                                             TrainingFcnType;

  MeasurementVectorType mv;
  TargetVectorType tv;
  TargetVectorType ov;
  ov.Fill(0.0);

  SampleType::Pointer trainsample = SampleType::New();
  SampleType::Pointer testsample = SampleType::New();
  TargetType::Pointer traintargets = TargetType::New();
  TargetType::Pointer testtargets = TargetType::New();
  trainsample->SetMeasurementVectorSize( num_input_nodes);
  traintargets->SetMeasurementVectorSize( num_output_nodes);
  testsample->SetMeasurementVectorSize( num_input_nodes);
  testtargets->SetMeasurementVectorSize( num_output_nodes);

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
    infile1 >> tv[0];
    trainsample->PushBack(mv);
    traintargets->PushBack(tv);
    std::cout << "Input =" << mv << std::endl;
    std::cout << "target =" << tv << std::endl;
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
    infile2 >> tv[0];
    testsample->PushBack(mv);
    testtargets->PushBack(tv);
    std::cout << "Input =" << mv << std::endl;
    std::cout << "target =" << tv << std::endl;
    }
  infile2.close();

  typedef itk::Statistics::OneHiddenLayerBackPropagationNeuralNetwork<MeasurementVectorType, TargetVectorType> OneHiddenLayerBackPropagationNeuralNetworkType;
  OneHiddenLayerBackPropagationNeuralNetworkType::Pointer net1 = OneHiddenLayerBackPropagationNeuralNetworkType::New();
  net1->SetNumOfInputNodes(num_input_nodes);
  net1->SetNumOfFirstHiddenNodes(num_hidden_nodes);
  net1->SetNumOfOutputNodes(num_output_nodes);

  net1->Initialize();
  net1->InitializeWeights();
  net1->SetLearningRate(0.01);

  TrainingFcnType::Pointer trainingfcn = TrainingFcnType::New();
  trainingfcn->SetIterations(20000);
  trainingfcn->SetThreshold(0.0001);
  trainingfcn->Train(net1, trainsample, traintargets);

  //Network Simulation
  std::cout << testsample->Size() << std::endl;
  std::cout << "Network Simulation" << std::endl;
  SampleType::ConstIterator iter1 = testsample->Begin();
  TargetType::ConstIterator iter2 = testtargets->Begin();
  unsigned int error1 = 0;
  unsigned int error2 = 0;
  int flag;
  std::ofstream outfile;
  outfile.open("out1.txt",std::ios::out);
  while (iter1 != testsample->End())
    {
    mv = iter1.GetMeasurementVector();
    tv = iter2.GetMeasurementVector();
    ov.SetVnlVector(net1->GenerateOutput(mv));
    flag = 0;
    if (std::fabs(tv[0]-ov[0])>0.2)
      {
      flag = 1;
      }
    if (flag == 1 && ROUND(tv[0]) == 1)
      {
      ++error1;
      }
    else if (flag == 1 && ROUND(tv[0]) == -1)
      {
      ++error2;
      }
    outfile<<mv[0]<<" "<<mv[1]<<" "<<tv[0]<<" "<<ov[0]<<std::endl;
    std::cout << "Network Input = " << mv << std::endl;
    std::cout << "Network Output = " << ov << std::endl;
    std::cout << "Target = " << tv << std::endl;
    ++iter1;
    ++iter2;
    }
  std::cout << "Among "<<num_test<<" measurement vectors, " << error1 + error2
            << " vectors are misclassified." << std::endl;
  std::cout<<"Network Weights and Biases after Training= "<<std::endl;
  std::cout << net1 << std::endl;

  if (double(error1 / 10) > 2 || double(error2 / 10) > 2)
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
