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

#include "itkTransformFileWriter.h"
#include "itkTransformFileReader.h"
#include "itkTransformFactory.h"
#include "itkTxtTransformIOFactory.h"
#include "itkEuler3DTransform.h"
#include "itkMath.h"

int itkIOEuler3DTransformTxtTest(int argc, char *argv[])
{
  if( argc != 3 )
    {
    std::cerr<< "Usage: "
             << argv[0]
             <<" inputFileName outputFileName"
             << std::endl;
    return EXIT_FAILURE;
    }

  itk::ObjectFactoryBase::RegisterFactory(itk::TxtTransformIOFactory::New() );

  typedef itk::Euler3DTransform<double> TransformType;
  TransformType::Pointer oldStyleInput, newStyleInput;

  typedef itk::TransformFileReaderTemplate<double> ReaderType;
  ReaderType::Pointer reader = ReaderType::New();

  typedef itk::TransformFileWriterTemplate<double> WriterType;
  WriterType::Pointer writer = WriterType::New();

  //read old style format in
  reader->SetFileName( argv[1] );
  reader->Update();
  oldStyleInput = static_cast< TransformType*  >(( reader->GetTransformList()->begin() )->GetPointer());

  //modify the interpretation of the Euler angles
  oldStyleInput->SetComputeZYX( true );

  //write in new style format
  writer->SetFileName( argv[2] );
  writer->SetInput( oldStyleInput );
  writer->Update();

  //read new style format back in
  reader->SetFileName( argv[2] );
  reader->Update();
  newStyleInput = static_cast< TransformType*  >(( reader->GetTransformList()->begin() )->GetPointer());

  const TransformType::MatrixType &oldStyleMat = oldStyleInput->GetMatrix();
  const TransformType::MatrixType &newStyleMat = newStyleInput->GetMatrix();

  if(!(itk::Math::FloatAlmostEqual(oldStyleMat(0,0), newStyleMat(0,0)) &&
       itk::Math::FloatAlmostEqual(oldStyleMat(0,1), newStyleMat(0,1)) &&
       itk::Math::FloatAlmostEqual(oldStyleMat(0,2), newStyleMat(0,2)) &&
       itk::Math::FloatAlmostEqual(oldStyleMat(1,0), newStyleMat(1,0)) &&
       itk::Math::FloatAlmostEqual(oldStyleMat(1,1), newStyleMat(1,1)) &&
       itk::Math::FloatAlmostEqual(oldStyleMat(1,2), newStyleMat(1,2)) &&
       itk::Math::FloatAlmostEqual(oldStyleMat(2,0), newStyleMat(2,0)) &&
       itk::Math::FloatAlmostEqual(oldStyleMat(2,1), newStyleMat(2,1)) &&
       itk::Math::FloatAlmostEqual(oldStyleMat(2,2), newStyleMat(2,2))))
    {
    std::cerr<< "Error reading new style format, different from data in memory." << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
