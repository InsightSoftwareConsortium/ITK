/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    NNetClassifierTest1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkOneHiddenLayerBackPropagationNeuralNetwork.h"
#include "itkIterativeSupervisedTrainingFunction.h"
#include "itkBatchSupervisedTrainingFunction.h"
#include "itkVector.h"
#include "itkListSample.h"
#include <vector>
#include <fstream>

#define ROUND(x) (floor(x+0.5))

int
NNetClassifierTest1(int argc, char* argv[])
{
  if (argc < 2)
    {
    std::cout << "ERROR: data file name argument missing." << std::endl ;
    return EXIT_FAILURE;
    }

  int num_train=800;
  int num_test=200;

  char* trainFileName = argv[1]; //"train.txt"; //argv[1];
  char* testFileName = argv[2]; //"test.txt"; //argv[2];

  const int num_input_nodes = 2;
  const int num_hidden_nodes = 2;
  const int num_output_nodes = 1;

  typedef itk::Vector<double, num_input_nodes> MeasurementVectorType;
  typedef itk::Vector<double, num_output_nodes> TargetVectorType;
  typedef itk::Statistics::ListSample<MeasurementVectorType> SampleType;
  typedef itk::Statistics::ListSample<TargetVectorType> TargetType;
  typedef itk::Statistics::BatchSupervisedTrainingFunction<SampleType, TargetType, double> TrainingFcnType;

  MeasurementVectorType mv;
  TargetVectorType tv;
  TargetVectorType ov;

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
  net1->SetNumOfHiddenNodes(num_hidden_nodes);
  net1->SetNumOfOutputNodes(num_output_nodes);

  net1->Initialize();
  net1->InitializeWeights();
  net1->SetLearningRate(0.001);

  TrainingFcnType::Pointer trainingfcn = TrainingFcnType::New();
  trainingfcn->SetIterations(200);
  trainingfcn->Train(net1, trainsample, traintargets);

  //Network Simulation
  std::cout << testsample->Size() << std::endl;
  std::cout << "Network Simulation" << std::endl;
  SampleType::ConstIterator iter1 = testsample->Begin();
  TargetType::ConstIterator iter2 = testtargets->Begin();
  unsigned int error1 = 0 ;
  unsigned int error2 = 0 ;
  int flag = 0;
  std::ofstream outfile;
  outfile.open("out1.txt",std::ios::out);
  while (iter1 != testsample->End())
    {
    mv = iter1.GetMeasurementVector();
    tv = iter2.GetMeasurementVector();
    ov = net1->GenerateOutput(mv);
    flag = 0;
    if (fabs(tv[0]-ov[0])>0.2)
      {
      outfile<<fabs(tv[0]-ov[0])<<std::endl;
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
    std::cout << "Network Input = " << mv << std::endl;
    std::cout << "Network Output = " << ov << std::endl;
    std::cout << "Target = " << tv << std::endl;
    ++iter1;
    ++iter2;
    }
  
  std::cout << "Among 500 measurement vectors, " << error1 + error2
            << " vectors are misclassified." << std::endl ;
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
