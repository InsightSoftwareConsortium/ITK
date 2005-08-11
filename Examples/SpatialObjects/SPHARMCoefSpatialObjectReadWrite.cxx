/*=========================================================================

  Author: Christine Xu

=========================================================================*/

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

//itkSpatialObjectReader/Writer.h have to be included before itkSPHARMCoefSpatialObject.h
#include <itkSpatialObjectReader.h>
#include <itkSpatialObjectWriter.h>

#include <itkSPHARMCoefSpatialObject.h>
#include <itkSPHARMCoefFileReader.h>

#include <iostream>

int main( int argc, char ** argv )
{
  if( argc < 3 )
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputCoefFile outputCoefSpatialObjectFile" << std::endl;
    return EXIT_FAILURE;
  }
  
  itk::SPHARMCoefFileReader::Pointer reader = itk::SPHARMCoefFileReader::New();
  try
  {
    std::cout<<"Opening "<<argv[1] <<"..." <<std::endl;
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
  
  typedef itk::SPHARMCoefSpatialObject::MeshTrait MeshTrait;
  typedef itk::SpatialObjectWriter<3,float,MeshTrait> WriterType;
  WriterType::Pointer writer = WriterType::New();
  
  std::cout<<"Writing " <<argv[2] << "... " ;
  writer->SetInput(coefobj);
  writer->SetFileName(argv[2]);
  writer->Update();
  std::cout<<"[PASSED]" <<std::endl;
  
  typedef itk::SpatialObjectReader<3,float,MeshTrait> ReaderType;
  ReaderType::Pointer SOreader = ReaderType::New();
  
  std::cout<<"Reading " <<argv[2] <<"... " ;
  SOreader->SetFileName(argv[2]);
  SOreader->Update();
  std::cout<<"[PASSED]" <<std::endl;
  
  return EXIT_SUCCESS;
  
}
