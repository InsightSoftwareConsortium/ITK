/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "itkMesh.h"
#include "itkPolyData.h"
#include "itkInputPolyData.h"
#include "itkOutputMesh.h"
#include "itkPipeline.h"
#include "itkSupportInputPolyDataTypes.h"
#include "itkWasmMeshIOFactory.h"
#include "itkPolyDataToMeshFilter.h"

template<typename TPolyData>
class PipelineFunctor
{
public:
  int operator()(itk::wasm::Pipeline & pipeline)
  {
    using PolyDataType = TPolyData;

    using InputPolyDataType = itk::wasm::InputPolyData<PolyDataType>;
    InputPolyDataType inputPolyData;
    pipeline.add_option("input-polydata", inputPolyData, "Input polydata")->required()->type_name("INPUT_POLYDATA");

    using MeshType = itk::Mesh<typename PolyDataType::PixelType, 3>;
    using OutputMeshType = itk::wasm::OutputMesh<MeshType>;
    OutputMeshType outputMesh;
    pipeline.add_option("output-mesh", outputMesh, "Output mesh")->required()->type_name("OUTPUT_MESH");

    ITK_WASM_PARSE(pipeline);

    using PolyDataToMeshFilterType = itk::PolyDataToMeshFilter<PolyDataType>;
    auto polyDataToMeshFilter = PolyDataToMeshFilterType::New();
    polyDataToMeshFilter->SetInput(inputPolyData.Get());
    polyDataToMeshFilter->Update();

    outputMesh.Set(polyDataToMeshFilter->GetOutput());

    return EXIT_SUCCESS;
  }
};

int main (int argc, char * argv[])
{
  itk::wasm::Pipeline pipeline("polydata-to-mesh", "Convert an itk::PolyData to an itk::Mesh", argc, argv);

  itk::WasmMeshIOFactory::RegisterOneFactory();

  return itk::wasm::SupportInputPolyDataTypes<PipelineFunctor>
  ::PixelTypes<uint8_t,int8_t,float,double>("input-polydata", pipeline);
}
