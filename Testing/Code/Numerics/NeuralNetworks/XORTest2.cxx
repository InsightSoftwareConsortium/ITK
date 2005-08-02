/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    XORTest2.cxx
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
XORTest2(int argc, char* argv[])
{
 if (argc < 1)
    {
      std::cout << "ERROR: data file name argument missing." << std::endl ;
      return EXIT_FAILURE;
    }
  char* dataFileName = argv[1] ;
  const int num_input_nodes = 2;
  const int num_hidden_nodes = 2;
  const int num_output_nodes = 1;

  typedef itk::Vector<double, num_input_nodes> MeasurementVectorType;
  typedef itk::Vector<double, num_output_nodes> TargetVectorType;
  typedef itk::Statistics::ListSample<MeasurementVectorType> SampleType;
  typedef itk::Statistics::ListSample<TargetVectorType> TargetType;

  typedef itk::Statistics::IterativeSupervisedTrainingFunction<SampleType, TargetType, double> TrainingFcnType;

  MeasurementVectorType mv;
  TargetVectorType tv;
  SampleType::Pointer sample = SampleType::New();
  TargetType::Pointer targets = TargetType::New();
  sample->SetMeasurementVectorSize( num_input_nodes);
  targets->SetMeasurementVectorSize( num_output_nodes);
 
  std::ifstream infile1;
  infile1.open(dataFileName, std::ios::in);

  infile1 >> mv[0] >> mv[1] >> tv[0];

  while (!infile1.eof())
    {
     std::cout << "Input =" << mv << std::endl;
     std::cout << "target =" << tv << std::endl;
     sample->PushBack(mv);
     targets->PushBack(tv);
     infile1 >> mv[0] >> mv[1] >> tv[0];    
    }
  infile1.close();

  std::cout << sample->Size() << std::endl;

  typedef itk::Statistics::OneHiddenLayerBackPropagationNeuralNetwork<MeasurementVectorType, TargetVectorType> OneHiddenLayerBackPropagationNeuralNetworkType;
  OneHiddenLayerBackPropagationNeuralNetworkType::Pointer net1 = OneHiddenLayerBackPropagationNeuralNetworkType::New();
  net1->SetNumOfInputNodes(num_input_nodes);
  net1->SetNumOfHiddenNodes(num_hidden_nodes);
  net1->SetNumOfOutputNodes(num_output_nodes);

  net1->Initialize();
  net1->InitializeWeights();
  net1->SetLearningRate(0.01);

  TrainingFcnType::Pointer trainingfcn = TrainingFcnType::New();
  trainingfcn->SetIterations(20000);
  
  trainingfcn->SetThreshold(0.001); 
  trainingfcn->Train(net1, sample, targets);

  //Network Simulation
  std::cout << sample->Size() << std::endl;
  std::cout << "Network Simulation" << std::endl;
  TargetVectorType ov;
  SampleType::ConstIterator iter1 = sample->Begin();
  TargetType::ConstIterator iter2 = targets->Begin();
  unsigned int error1 = 0 ;
  unsigned int error2 = 0 ;
  int flag = 0;
  std::ofstream outfile;
  outfile.open("out1.txt",std::ios::out);
  while (iter1 != sample->End())
    {
      mv = iter1.GetMeasurementVector();
      tv = iter2.GetMeasurementVector();
      ov = net1->GenerateOutput(mv);
      flag = 0;
      if (fabs(tv[0]-ov[0])>0.2)
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
  std::cout << "Among 4 measurement vectors, " << error1 + error2
            << " vectors are misclassified." << std::endl ;
  std::cout<<"Network Weights and Biases after Training= "<<std::endl;
  std::cout << net1 << std::endl;
  if ((error1 + error2) > 2)
    {
      std::cout << "Test failed." << std::endl;
      return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
