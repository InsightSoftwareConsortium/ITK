/*=========================================================================

  Author: Christine Xu

=========================================================================*/

#include "itkMeshSpatialObject.h"
#include "itkSpatialObjectWriter.h"
#include "itkSpatialObjectReader.h"
#include "itkMesh.h"
#include <iostream>
#include <string>

#include "itkSPHARMCoefSpatialObject.h"
#include "itkSPHARMCoefFileReader.h"
#include "itkSphericalHarmonicMeshSource.h"


int main( int argc, char ** argv )
{
   if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputCoefFile outputMeshSpatialObjectFile" << std::endl;
    return EXIT_FAILURE;
    }
    
  
  itk::SPHARMCoefFileReader::Pointer reader = itk::SPHARMCoefFileReader::New();
  try
  {
    std::cerr<<"Opening "<<argv[1] <<"..." <<std::endl;
    reader->SetFileName(argv[1]);
    reader->Update();
  }
  catch(itk::SPHARMCoefFileReaderException ex)
  {
    std::cerr<<ex.GetDescription()<<std::endl;

    return EXIT_FAILURE;
  }
  
  itk::SPHARMCoefSpatialObject::Pointer coefobj = itk::SPHARMCoefSpatialObject::New();
  itk::SPHARMCoefSpatialObject::CoefListType coeflist;
  reader->GetOutput(coeflist);
  coefobj->SetCoefs(coeflist);
  
  
  // Create an itkMesh
  std::cout << "Creating Mesh File: " << std::endl;
  itk::SphericalHarmonicMeshSource::Pointer meshsrc = itk::SphericalHarmonicMeshSource::New();
  meshsrc->SetCoefs(coeflist);
  meshsrc->Update();
  typedef itk::SphericalHarmonicMeshSource::OutputMeshType OutputMeshType;
  OutputMeshType* meshSH;
  try
  {
    meshSH = meshsrc->GetOutput();
  }
  catch(itk::SphericalHarmonicMeshSourceException ex)
  {
    std::cerr<< ex.GetDescription() <<std::endl;
    
    return EXIT_FAILURE;
  }
  
  std::cout << "Points = " << meshSH->GetNumberOfPoints() << std::endl;
  std::cout << "Cells  = " << meshSH->GetNumberOfCells()  << std::endl;
  
  typedef itk::MeshSpatialObject<OutputMeshType>        MeshSHType;
  typedef OutputMeshType::MeshTraits                    MeshSHTrait;
 
  // Create the mesh Spatial Object
  std::cout << "Creating Mesh Spatial Object... ";
  MeshSHType::Pointer meshSHSO = MeshSHType::New();
  meshSHSO->SetMesh(meshSH);
  std::cout<<"[PASSED]"<<std::endl;
  
  std::cout << "Creating Scene Object... ";
  typedef itk::SceneSpatialObject<3> SceneType;
  SceneType::Pointer scene = SceneType::New();
  scene->AddSpatialObject(meshSHSO);
  
  std::cout << "NObject = " << scene->GetNumberOfObjects() ;
  std::cout<<"[PASSED]"<<std::endl;  
  
  // Writing the file
  std::cout<<"Testing Writing MeshSpatialObject... ";
  typedef itk::SpatialObjectWriter<3,float,MeshSHTrait> WriterSHType;
  WriterSHType::Pointer writerSH = WriterSHType::New();
  try
  {
    writerSH->SetInput(scene);
    writerSH->SetFileName(argv[2]);
    writerSH->Update();  
  }
  catch(itk::ExceptionObject ex)
  {
    std::cerr<<ex.GetDescription()<<std::endl;

    return EXIT_FAILURE;
  }
  std::cout<<"[PASSED]"<<std::endl;
  
  std::cout << "[TEST DONE]" << std::endl;
  
  return EXIT_SUCCESS;
}

