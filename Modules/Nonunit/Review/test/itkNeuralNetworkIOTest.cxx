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

#include "itkNeuralNetworkFileReader.h"
#include "itkNeuralNetworkFileWriter.h"
#include "itkIterativeSupervisedTrainingFunction.h"
#include "itkListSample.h"

#include <iostream>

int itkNeuralNetworkIOTest(int argc,char* argv[])
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] <<
      " NetworkConfigurationFile  TrainingData TemporaryFileLocation" << std::endl;
    return EXIT_FAILURE;
    }
  const std::string XORNetFileName(argv[1]);
  const std::string dataFileName(argv[2]);
  const std::string tempDataDirectory(argv[3]);

  const unsigned int num_input_nodes=2;
  const unsigned int num_output_nodes=1;
  typedef itk::Vector<double, num_input_nodes>   MeasurementVectorType;
  typedef itk::Vector<double, num_output_nodes>  TargetVectorType;

#if 1
  typedef itk::Statistics::MultilayerNeuralNetworkBase<
    MeasurementVectorType, TargetVectorType> NetworkType;

  typedef itk::Statistics::ListSample<MeasurementVectorType>    SampleType;
  typedef itk::Statistics::ListSample<TargetVectorType>         TargetType;

  typedef itk::Statistics::IterativeSupervisedTrainingFunction<
    SampleType, TargetType, double> TrainingFcnType;

  typedef itk::NeuralNetworkFileReader<NetworkType> ReaderType;

  typedef itk::NeuralNetworkFileWriter<NetworkType> WriterType;

  ReaderType::Pointer reader=ReaderType::New();

  //exercise Set/GetFilename method for code coverage
  std::string testName = tempDataDirectory+std::string("/Input.txt");
  reader->SetFileName( testName );


  if ( reader->GetFileName() != testName )
    {
    std::cerr << "Error in Set/Get Filename:" << std::endl;
    return EXIT_FAILURE;
    }

  //exercise Set/GetFilename method for code coverage
  reader->SetReadWeightValuesType( ReaderType::ASCII );

  if ( reader->GetReadWeightValuesType() != ReaderType::ASCII )
    {
    std::cerr << "Error in Set/Get ReadWeightValuesType:" << std::endl;
    return EXIT_FAILURE;
    }

  reader->SetReadWeightValuesType( ReaderType::IGNORE );

  // Read the Network topology from the configuration file
  reader->SetFileName(XORNetFileName);

  reader->Update();
  NetworkType::Pointer network = reader->GetOutput();

  // Initialize network
  network->Initialize();
  std::cout << "________Network after read from __________" << XORNetFileName << std::endl;
  std::cout << network << std::endl;

  // Read in training data
  MeasurementVectorType mv;
  TargetVectorType tv;
  SampleType::Pointer sample = SampleType::New();
  TargetType::Pointer targets = TargetType::New();
  sample->SetMeasurementVectorSize( num_input_nodes);
  targets->SetMeasurementVectorSize( num_output_nodes);
  std::ifstream infile1;
  infile1.open(dataFileName.c_str(), std::ios::in);

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
  ov.Fill(0.0);
  SampleType::ConstIterator iter1 = sample->Begin();
  TargetType::ConstIterator iter2 = targets->Begin();
  unsigned int error1 = 0;
  unsigned int error2 = 0;
  int flag = 0;

  while( iter1 != sample->End() )
    {
    mv = iter1.GetMeasurementVector();
    tv = iter2.GetMeasurementVector();
    ov.SetVnlVector(network->GenerateOutput(mv));
    flag = 0;
    if( itk::Math::abs(tv[0]-ov[0])>0.5 && !((tv[0]*ov[0])>0) )
      {
      flag = 1;
      }
    if( flag == 1 && std::floor(tv[0]+0.5) )
      {
      ++error1;
      }
    else if (flag == 1 && std::floor(tv[0]+0.5) == -1)
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

  //exercise Set/GetFilename method for code coverage
  const std::string testNameOutput = tempDataDirectory+std::string("/Output.txt");
  writer->SetFileName( testNameOutput );

  if ( writer->GetFileName() != testNameOutput )
    {
    std::cerr << "Error in Set/Get Filename:" << std::endl;
    return EXIT_FAILURE;
    }

  //exercise Set/Get WriteWeightValuesType
  writer->SetWriteWeightValuesType( WriterType::ASCII );

  if ( writer->GetWriteWeightValuesType() != WriterType::ASCII )
    {
    std::cerr << "Error in Set/Get WriteWeightValuesType:" << std::endl;
    return EXIT_FAILURE;
    }

  writer->SetWriteWeightValuesType(WriterType::ASCII);
  writer->SetFileName(tempDataDirectory+std::string("/xornetASCII.txt"));
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

    {
    WriterType::Pointer writer2=WriterType::New();
    writer2->SetWriteWeightValuesType(WriterType::BINARY);
    writer2->SetFileName(tempDataDirectory+std::string("/xornetBinary.txt"));
    writer2->SetInput(network);
    writer2->Update();

    if( (error1 + error2) > 2 )
      {
      std::cout << "Test failed." << std::endl;
      return EXIT_FAILURE;
      }
    }
#endif
  //Now test reading and writing of OneHiddenLayerBackPropagationNeuralNetwork
    {
    const std::string TestOneHiddenLayerNetFileName=tempDataDirectory+std::string("/OneHiddenLayerNet.txt");
    typedef itk::Statistics::OneHiddenLayerBackPropagationNeuralNetwork<MeasurementVectorType, TargetVectorType> OneHiddenLayerBackPropagationNeuralNetworkType;
    OneHiddenLayerBackPropagationNeuralNetworkType::Pointer OneHiddenLayerNet = OneHiddenLayerBackPropagationNeuralNetworkType::New();
    OneHiddenLayerNet->SetNumOfInputNodes(2);
    OneHiddenLayerNet->SetNumOfFirstHiddenNodes(2);
    OneHiddenLayerNet->SetNumOfOutputNodes(1);


    OneHiddenLayerNet->InitializeWeights();
    OneHiddenLayerNet->SetLearningRate(0.001);
    OneHiddenLayerNet->Initialize();
    std::cout << "___________________________________OneHiddenLayerNet: " << TestOneHiddenLayerNetFileName << std::endl;
    std::cout << OneHiddenLayerNet << std::endl;
    std::cout << "___________________________________OneHiddenLayerNet: " << TestOneHiddenLayerNetFileName << std::endl;
      {
      typedef itk::NeuralNetworkFileWriter<OneHiddenLayerBackPropagationNeuralNetworkType> OHLWriterType;
      OHLWriterType::Pointer writerOneHiddenLayerBackPropagation=OHLWriterType::New();
      writerOneHiddenLayerBackPropagation->SetWriteWeightValuesType(OHLWriterType::ASCII);
      writerOneHiddenLayerBackPropagation->SetFileName(TestOneHiddenLayerNetFileName);
      writerOneHiddenLayerBackPropagation->SetInput(OneHiddenLayerNet);
      writerOneHiddenLayerBackPropagation->Update();
      }
      {
      typedef itk::NeuralNetworkFileReader<OneHiddenLayerBackPropagationNeuralNetworkType> OHLReaderType;
      OHLReaderType::Pointer readerOneHiddenLayerBackPropagation=OHLReaderType::New();
      readerOneHiddenLayerBackPropagation->SetFileName(TestOneHiddenLayerNetFileName);
      readerOneHiddenLayerBackPropagation->SetReadWeightValuesType( OHLReaderType::ASCII );
      readerOneHiddenLayerBackPropagation->Update();
      //The following line gives a compiler error
      OneHiddenLayerBackPropagationNeuralNetworkType::Pointer OneHiddenLayerNet_ReadIn = readerOneHiddenLayerBackPropagation->GetOutput();
      }
    }
  //Now test reading and writing of TwoHiddenLayerBackPropagationNeuralNetwork
    {
    const std::string TestTwoHiddenLayerNetFileName=tempDataDirectory+std::string("/TwoHiddenLayerNet.txt");
    typedef itk::Statistics::TwoHiddenLayerBackPropagationNeuralNetwork<MeasurementVectorType, TargetVectorType> TwoHiddenLayerBackPropagationNeuralNetworkType;
    TwoHiddenLayerBackPropagationNeuralNetworkType::Pointer TwoHiddenLayerNet = TwoHiddenLayerBackPropagationNeuralNetworkType::New();
    TwoHiddenLayerNet->SetNumOfInputNodes(7);
    TwoHiddenLayerNet->SetNumOfFirstHiddenNodes(5);
    TwoHiddenLayerNet->SetNumOfSecondHiddenNodes(3);
    TwoHiddenLayerNet->SetNumOfOutputNodes(1);

    typedef itk::NeuralNetworkFileWriter<TwoHiddenLayerBackPropagationNeuralNetworkType> OHLWriterType;

    TwoHiddenLayerNet->InitializeWeights();
    TwoHiddenLayerNet->SetLearningRate(0.001);
    TwoHiddenLayerNet->Initialize();
    std::cout << "___________________________________TwoHiddenLayerNet: " << TestTwoHiddenLayerNetFileName << std::endl;
    std::cout << TwoHiddenLayerNet << std::endl;
    std::cout << "___________________________________TwoHiddenLayerNet: " << TestTwoHiddenLayerNetFileName << std::endl;
      {
      OHLWriterType::Pointer writerTwoHiddenLayerBackPropagation=OHLWriterType::New();
      writerTwoHiddenLayerBackPropagation->SetWriteWeightValuesType(OHLWriterType::ASCII);
      writerTwoHiddenLayerBackPropagation->SetFileName(TestTwoHiddenLayerNetFileName);
      writerTwoHiddenLayerBackPropagation->SetInput(TwoHiddenLayerNet);
      writerTwoHiddenLayerBackPropagation->Update();
      }
      {
      typedef itk::NeuralNetworkFileReader<TwoHiddenLayerBackPropagationNeuralNetworkType> OHLReaderType;
      OHLReaderType::Pointer readerTwoHiddenLayerBackPropagation=OHLReaderType::New();
      readerTwoHiddenLayerBackPropagation->SetFileName(TestTwoHiddenLayerNetFileName);
      readerTwoHiddenLayerBackPropagation->SetReadWeightValuesType( OHLReaderType::ASCII );
      readerTwoHiddenLayerBackPropagation->Update();
      //The following line gives a compiler error
      TwoHiddenLayerBackPropagationNeuralNetworkType::Pointer TwoHiddenLayerNet_ReadIn = readerTwoHiddenLayerBackPropagation->GetOutput();
      }
    }
  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
