#include "iostream"

#include "itkVideoFileReader.h"
#include "itkVideoFileWriter.h"


int test_pipeline (char* Input, char* Output,bool readerUseOpenCV, bool writerUseOpenCV)
{
  typedef itk::Image< unsigned char, 2>   OutputImageType;  
  itk::VideoFileReader< OutputImageType >::Pointer Reader = itk::VideoFileReader< OutputImageType >::New();
  Reader->SetFileName(Input);
  Reader->UseOpenCV(readerUseOpenCV);

  Reader->LoadVideo();

  unsigned long i;

  itk::VideoFileWriter<OutputImageType>::Pointer VideoWriter = itk::VideoFileWriter<OutputImageType>::New();
  VideoWriter->SetInput(Reader->GetOutput());
  VideoWriter->SetFileName(Output);
  VideoWriter->UseOpenCV(writerUseOpenCV);

  for (i = 0; i < Reader->GetFrameTotal()-1 ; i ++ )
    {
    
    try
      {
      VideoWriter->Update();
      }
    catch (itk::ExceptionObject &e)
      {
      VideoWriter->Print(std::cout);
      std::cerr<<e.GetFile()<<std::endl;
      std::cerr<<e.GetLine()<<std::endl;
      std::cerr<<e.GetLocation()<<std::endl;
      std::cerr<<e.GetNameOfClass()<<std::endl;
      std::cerr<<e.GetDescription()<<std::endl;
      return EXIT_FAILURE;
      }

    Reader->KeepReading();
    }

  VideoWriter->EndVideo();

  std::cout<<"Done !"<<std::endl;
  return EXIT_SUCCESS;
}

int main (int argv, char **argc)
{
  return test_pipeline(argc[1],argc[2],atoi(argc[3]),atoi(argc[4]));
}