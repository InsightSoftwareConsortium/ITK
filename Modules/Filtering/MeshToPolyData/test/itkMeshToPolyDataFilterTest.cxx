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
#include "itkMath.h"

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

  using MeshReaderType = itk::MeshFileReader< MeshType >;
  MeshReaderType::Pointer meshReader = MeshReaderType::New();
  meshReader->SetFileName( inputMeshFileName );
  meshReader->Update();

  using FilterType = itk::MeshToPolyDataFilter< MeshType >;
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, MeshToPolyDataFilter, ProcessObject );

  filter->SetInput( meshReader->GetOutput() );

  ShowProgress::Pointer showProgress = ShowProgress::New();
  filter->AddObserver( itk::ProgressEvent(), showProgress );

  TRY_EXPECT_NO_EXCEPTION( filter->Update() );

  using PolyDataType = FilterType::PolyDataType;
  PolyDataType::ConstPointer polyData = filter->GetOutput();

  TEST_EXPECT_EQUAL( polyData->GetNumberOfPoints(), 2903 );

  PolyDataType::PointsContainer::ConstPointer points = polyData->GetPoints();
  TEST_EXPECT_TRUE( itk::Math::FloatAlmostEqual< float >( points->GetElement( 0 )[0], 3.71636, 10, 1e-4 ) );
  TEST_EXPECT_TRUE( itk::Math::FloatAlmostEqual< float >( points->GetElement( 0 )[1], 2.34339, 10, 1e-4 ) );
  TEST_EXPECT_TRUE( itk::Math::FloatAlmostEqual< float >( points->GetElement( 0 )[2], 0.0, 10, 1e-4 ) );

  TEST_EXPECT_EQUAL( polyData->GetVertices()->size(), 0 );

  TEST_EXPECT_EQUAL( polyData->GetLines()->size(), 0 );

  TEST_EXPECT_EQUAL( polyData->GetPolygons()->size(), 15593 );
  TEST_EXPECT_EQUAL( polyData->GetPolygons()->GetElement( 0 ), 4 );
  TEST_EXPECT_EQUAL( polyData->GetPolygons()->GetElement( 1 ), 250 );
  TEST_EXPECT_EQUAL( polyData->GetPolygons()->GetElement( 2 ), 251 );
  TEST_EXPECT_EQUAL( polyData->GetPolygons()->GetElement( 3 ), 210 );
  TEST_EXPECT_EQUAL( polyData->GetPolygons()->GetElement( 4 ), 252 );
  TEST_EXPECT_EQUAL( polyData->GetPolygons()->GetElement( 5 ), 4 );
  TEST_EXPECT_EQUAL( polyData->GetPolygons()->GetElement( 6 ), 252 );

  return EXIT_SUCCESS;
}
