/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeuralNetworkIOTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif


#include "itkNeuralNetworkFileReader.h"
#include "itkNeuralNetworkFileWriter.h"
#include "itkIterativeSupervisedTrainingFunction.h"
#include "itkListSample.h"
#include "itkVector.h"

#include <iostream>

int itkNeuralNetworkIOTest(int argc,char* argv[])
{
  if( argc < 2 )
  {
    std::cerr << "Usage: " << argv[0] << 
      " NetworkConfigurationFile  TrainingData" << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int num_input_nodes=2;
  const unsigned int num_output_nodes=1;
  typedef itk::Vector<double, num_input_nodes>   MeasurementVectorType;
  typedef itk::Vector<double, num_output_nodes>  TargetVectorType;

  typedef itk::Statistics::MultilayerNeuralNetworkBase<
                          MeasurementVectorType, TargetVectorType> NetworkType;

  typedef itk::Statistics::ListSample<MeasurementVectorType>    SampleType;
  typedef itk::Statistics::ListSample<TargetVectorType>         TargetType;

  typedef itk::Statistics::IterativeSupervisedTrainingFunction<
                          SampleType, TargetType, double> TrainingFcnType;

  char* dataFileName = argv[2];

  typedef itk::NeuralNetworkFileReader<
                       MeasurementVectorType,TargetVectorType> ReaderType;

  typedef itk::NeuralNetworkFileWriter<
                       MeasurementVectorType,TargetVectorType> WriterType;

  ReaderType::Pointer reader=ReaderType::New(); 
  
  //exercise Set/GetFilename method for code coverage
  std::string testName = "Input.txt";
  reader->SetFileName( testName );

  if ( reader->GetFileName() != testName ) 
    {
    std::cerr << "Error in Set/Get Filename:" << std::endl;
    return EXIT_FAILURE; 
    } 

  //exercise Set/GetFilename method for code coverage
  reader->SetReadWeightValuesType( 1 ); 

  if ( reader->GetReadWeightValuesType() != 1 ) 
    {
    std::cerr << "Error in Set/Get ReadWeightValuesType:" << std::endl;
    return EXIT_FAILURE; 
    } 

  reader->SetReadWeightValuesType( 0 );

  // Read the Network topology from the configuration file
  reader->SetFileName(argv[1]);

  reader->Update();

  NetworkType::Pointer network = reader->GetOutput();

  // Initialize network
  network->Initialize();

  // Read in training data
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


  //Network Simulation
  std::cout << sample->Size() << std::endl;
  std::cout << "Network Simulation" << std::endl;
  TargetVectorType ov;
  SampleType::ConstIterator iter1 = sample->Begin();
  TargetType::ConstIterator iter2 = targets->Begin();
  unsigned int error1 = 0;
  unsigned int error2 = 0;
  int flag = 0;

  while( iter1 != sample->End() )
    {
    mv = iter1.GetMeasurementVector();
    tv = iter2.GetMeasurementVector();
    ov.Set_vnl_vector(network->GenerateOutput(mv));
    flag = 0;
    if( fabs(tv[0]-ov[0])>0.5 && !((tv[0]*ov[0])>0) )
      {
      flag = 1;
      }
    if( flag == 1 && vcl_floor(tv[0]+0.5) )
      {
      ++error1;
      }
    else if (flag == 1 && vcl_floor(tv[0]+0.5) == -1)
      {
      ++error2;
      }

    std::cout << "Network Input = " << mv << std::endl;
    std::cout << "Network Output = " << ov << std::endl;
    std::cout << "Target = " << tv << std::endl;
    ++iter1;
    ++iter2;
    }

  std::cout << "Among 4 measurement vectors, " << error1 + error2
            << " vectors are misclassified." << std::endl;
  std::cout << "Network Weights and Biases after Training= " << std::endl;
  std::cout << network << std::endl;


  //Write out network as it was read in

  WriterType::Pointer writer=WriterType::New();
  writer->SetWriteWeightValuesType(1);
  writer->SetFileName("xornetASCII.txt");
  writer->SetInput(network);

  if( writer->GetInput() != network )
    {
    std::cerr << "Error in SetInput()/GetInput() " << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  //Reinitialize network and train

  network->InitializeWeights();
  TrainingFcnType::Pointer trainingfcn = TrainingFcnType::New();
  trainingfcn->SetIterations(2000);
  trainingfcn->SetThreshold(0.001); 
  trainingfcn->Train(network, sample, targets);

  WriterType::Pointer writer2=WriterType::New();
  writer2->SetWriteWeightValuesType(2);
  writer2->SetFileName("xornetBinary.txt");
  writer2->SetInput(network);
  writer2->Update();

  if( (error1 + error2) > 2 )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
