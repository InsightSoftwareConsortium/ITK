#include "iostream"

#include "itkVideoFileReader.h"
#include "itkImageFileWriter.h"


// This is an example that illustate how the Light video file reader works
// It basically saves a image from a video every minute. (every 1500 frame
// if the frame period is 1/25 s)

int test_reader ( char* Input, char* OutputWithoutExtension, bool readerUseOpenCV )
{
  typedef itk::Image<unsigned char, 2>   OutputImageType;  
  itk::VideoFileReader< OutputImageType >::Pointer reader = itk::VideoFileReader< OutputImageType >::New();
  reader->SetFileName(Input);
  reader->UseOpenCV(readerUseOpenCV);
  reader->LoadVideo();
  
  unsigned int FrameTotal = reader->GetFrameTotal();
  unsigned int i;
  char* buf =  new char;

  itk::ImageFileWriter<OutputImageType>::Pointer writer = itk::ImageFileWriter<OutputImageType>::New();
  writer->SetInput(reader->GetOutput());

  for (i  = 0; i < FrameTotal ; i++)
    {
    if ( i % 500 == 0 )
      {
      //To set a different filename each time
      std::string filename = OutputWithoutExtension;
      sprintf( buf, "%d", i );
      filename += std::string(buf);
      filename += ".png";
      writer->SetFileName(filename.c_str());
      try
        {
        writer->Update();
        }
      catch (itk::ExceptionObject &e)
        {
        reader->Print(std::cout);
        writer->Print(std::cout);
        std::cerr<<e.GetFile()<<std::endl;
        std::cerr<<e.GetLine()<<std::endl;
        std::cerr<<e.GetLocation()<<std::endl;
        std::cerr<<e.GetNameOfClass()<<std::endl;
        std::cerr<<e.GetDescription()<<std::endl;
        return EXIT_FAILURE;
        }
      }
    else
      {
      reader->Update();
      }

    reader->KeepReading();
    }

  std::string filename = OutputWithoutExtension ;
  filename += "Half.png";
  reader->SetNextFrameIsFrameRequested(true);
  reader->SetFrameRequested(static_cast<unsigned long>(FrameTotal/2));
  
  writer->SetFileName(filename.c_str());
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    reader->Print(std::cout);
    writer->Print(std::cout);
    std::cerr<<e.GetFile()<<std::endl;
    std::cerr<<e.GetLine()<<std::endl;
    std::cerr<<e.GetLocation()<<std::endl;
    std::cerr<<e.GetNameOfClass()<<std::endl;
    std::cerr<<e.GetDescription()<<std::endl;
    return EXIT_FAILURE;
    }

  std::cout<<"Done !"<<std::endl;
  return EXIT_SUCCESS;
}

int main ( int argc, char *argv[] )
{
  /*int k = test_reader("C:/projects/ITK-Vid-A2D2/Data/inde-circulation.avi", 
    "C:/projects/ITK-Vid-A2D2_build/Testing/OpenCVFrame_Number_", 
    atoi("1"));
  std::cin>>k;
  return k;*/

  return test_reader(argv[1],argv[2],atoi(argv[3]));
}

