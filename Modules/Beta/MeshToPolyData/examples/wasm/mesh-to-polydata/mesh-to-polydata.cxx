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
#include "itkInputMesh.h"
#include "itkOutputPolyData.h"
#include "itkPipeline.h"
#include "itkSupportInputMeshTypes.h"
#include "itkWasmMeshIOFactory.h"
#include "itkMeshToPolyDataFilter.h"
#include "itkVector.h"

template<typename TMesh>
class PipelineFunctor
{
public:
  int operator()(itk::wasm::Pipeline & pipeline)
  {
    using MeshType = TMesh;

    using InputMeshType = itk::wasm::InputMesh<MeshType>;
    InputMeshType inputMesh;
    pipeline.add_option("input-mesh", inputMesh, "Input mesh")->required()->type_name("INPUT_MESH");

    using PolyDataType = itk::PolyData<typename MeshType::PixelType>;
    using OutputPolyDataType = itk::wasm::OutputPolyData<PolyDataType>;
    OutputPolyDataType outputPolyData;
    pipeline.add_option("output-polydata", outputPolyData, "Output polydata")->required()->type_name("OUTPUT_POLYDATA");

    ITK_WASM_PARSE(pipeline);

    using MeshToPolyDataFilterType = itk::MeshToPolyDataFilter<MeshType>;
    auto meshToPolyDataFilter = MeshToPolyDataFilterType::New();
    meshToPolyDataFilter->SetInput(inputMesh.Get());
    meshToPolyDataFilter->Update();

    outputPolyData.Set(meshToPolyDataFilter->GetOutput());

    return EXIT_SUCCESS;
  }
};

int main (int argc, char * argv[])
{
  itk::wasm::Pipeline pipeline("mesh-to-polydata", "Convert an itk::Mesh to an itk::PolyData", argc, argv);

  itk::WasmMeshIOFactory::RegisterOneFactory();

  return itk::wasm::SupportInputMeshTypes<PipelineFunctor,
   uint8_t,
   int8_t,
   float,
   double,
   itk::Vector<uint8_t, 3>,
   itk::Vector<float, 3>
   >
  ::Dimensions<2U,3U>("input-mesh", pipeline);
}
