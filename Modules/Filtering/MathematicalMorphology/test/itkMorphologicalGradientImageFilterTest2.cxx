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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMorphologicalGradientImageFilter.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkSimpleFilterWatcher.h"
#include <fstream>

int itkMorphologicalGradientImageFilterTest2(int argc, char * argv[])
{
  if( argc < 3 )
  {
    std::cerr << "Missing Arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImage outputImage " << std::endl;
    return EXIT_FAILURE;
  }

  const int dim = 2;

  typedef unsigned char            PType;
  typedef itk::Image< PType, dim > IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  typedef itk::BinaryBallStructuringElement< PType, dim  > StructuringElementType;
  StructuringElementType  structuringElement;
  structuringElement.SetRadius( 2 );
  structuringElement.CreateStructuringElement();

  typedef itk::MorphologicalGradientImageFilter< IType, IType, StructuringElementType > GradientType;
  GradientType::Pointer gradient = GradientType::New();
  gradient->SetInput(reader->GetOutput());
  gradient->SetKernel( structuringElement );
  itk::SimpleFilterWatcher watcher(gradient);

  const int algorithmType = gradient->GetAlgorithm();
  std::cout<<"algorithmType : "<<algorithmType<<std::endl;

  typedef itk::ImageFileWriter< IType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(gradient->GetOutput());
  writer->SetFileName(argv[2]);

  try
  {
  writer->Update();
  }
  catch( itk::ExceptionObject & excp )
  {
    std::cerr << "Exception caught ! " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  try
  {
    structuringElement.SetRadius( 4 );
    gradient->SetAlgorithm(0);
    gradient->Update();
    const int algorithmType1 = gradient->GetAlgorithm();
    std::cout<<"algorithmType1 : "<<algorithmType1<<std::endl;
  }
  catch (itk::ExceptionObject& e)
  {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return EXIT_FAILURE;
  }

  try
  {
    typedef itk::FlatStructuringElement<dim> SRType;
    SRType::RadiusType elementRadius;
    elementRadius.Fill(4);
    SRType structuringElement2 = SRType::Box(elementRadius);
    typedef itk::MorphologicalGradientImageFilter< IType, IType, SRType > Gradient1Type;
    Gradient1Type::Pointer gradient1 = Gradient1Type::New();
    gradient1->SetInput(reader->GetOutput());
    gradient1->SetKernel( structuringElement2 );
    gradient1->SetAlgorithm(3);
    gradient1->Update();
    const int algorithmType2 = gradient1->GetAlgorithm();
    std::cout<<"algorithmType : "<<algorithmType2<<std::endl;

    gradient1->SetAlgorithm(2);
    gradient1->Update();
    const int algorithmType3 = gradient1->GetAlgorithm();
    std::cout<<"algorithmType : "<<algorithmType3<<std::endl;
  }
  catch (itk::ExceptionObject& e)
  {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
