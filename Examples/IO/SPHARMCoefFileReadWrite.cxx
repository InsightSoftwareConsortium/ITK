/*=========================================================================

  Author: Christine Xu

=========================================================================*/

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif 
 
#include "itkSPHARMCoefSpatialObject.h"
#include "itkSPHARMCoefFileReader.h"
#include "itkSPHARMCoefFileWriter.h"

#include <iostream>


int main( int argc, char ** argv )
{
   
   if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputCoefFile outputCoefFile" << std::endl;
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
  coefobj->PrintSelf(std::cout, itk::Indent(0));
  

  itk::SPHARMCoefFileWriter::Pointer writer = itk::SPHARMCoefFileWriter::New();
  try
  {
     std::cout<<"Writing "<<argv[2] <<"..." <<std::endl;
    writer->SetFileName(argv[2]);
    writer->SetInput(coeflist);
    writer->Update();
  }
  catch(itk::SPHARMCoefFileWriterException ex)
  {
    std::cerr<<ex.GetDescription()<<std::endl;
    
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
