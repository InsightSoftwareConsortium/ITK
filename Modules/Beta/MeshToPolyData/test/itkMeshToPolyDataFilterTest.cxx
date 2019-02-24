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

#include "itkMeshToPolyDataFilter.h"

#include "itkCommand.h"
#include "itkMeshFileReader.h"
#include "itkMesh.h"
#include "itkTestingMacros.h"

namespace{
class ShowProgress : public itk::Command
{
public:
  itkNewMacro( ShowProgress );

  void
  Execute( itk::Object* caller, const itk::EventObject& event ) override
  {
    Execute( (const itk::Object*)caller, event );
  }

  void
  Execute( const itk::Object* caller, const itk::EventObject& event ) override
  {
    if ( !itk::ProgressEvent().CheckEvent( &event ) )
      {
      return;
      }
    const auto* processObject = dynamic_cast< const itk::ProcessObject* >( caller );
    if ( !processObject )
      {
      return;
      }
    std::cout << " " << processObject->GetProgress();
  }
};
}


int itkMeshToPolyDataFilterTest( int argc, char * argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputMesh outputPolyData";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }
  const char * inputMeshFileName = argv[1];
  const char * outputPolyDataFileName = argv[1];

  const unsigned int Dimension = 3;
  using PixelType = float;
  using MeshType = itk::Mesh< PixelType, Dimension >;

  using FilterType = itk::MeshToPolyDataFilter< MeshType >;
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, MeshToPolyDataFilter, ProcessObject );

  //// Create input image to avoid test dependencies.
  //ImageType::SizeType size;
  //size.Fill( 128 );
  //ImageType::Pointer image = ImageType::New();
  //image->SetRegions( size );
  //image->Allocate();
  //image->FillBuffer(1.1f);

  //ShowProgress::Pointer showProgress = ShowProgress::New();
  //filter->AddObserver( itk::ProgressEvent(), showProgress );
  //filter->SetInput(image);

  //typedef itk::ImageFileWriter< ImageType > WriterType;
  //WriterType::Pointer writer = WriterType::New();
  //writer->SetFileName( outputImageFileName );
  //writer->SetInput( filter->GetOutput() );
  //writer->SetUseCompression(true);

  //TRY_EXPECT_NO_EXCEPTION( writer->Update() );


  return EXIT_SUCCESS;
}
