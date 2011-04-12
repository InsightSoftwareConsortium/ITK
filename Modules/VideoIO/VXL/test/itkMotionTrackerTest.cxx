#include <iostream>
#include <stdio.h>
#include <stdlib.h>

#include "itkVideoFileReader.h"
#include "itkVideoFileWriter.h"
#include "itkDifferenceImageFilter.h"


int motion_tracker (char* Input, char* Output,bool readerUseOpenCV, bool writerUseOpenCV)
{

  std::cout << "Setting up file reader..." << std::endl;

  typedef itk::Image< unsigned char, 2>   OutputImageType;  
  itk::VideoFileReader< OutputImageType >::Pointer TestReader = itk::VideoFileReader< OutputImageType >::New();
  TestReader->SetFileName(Input);
  TestReader->UseOpenCV(readerUseOpenCV);
  itk::VideoFileReader< OutputImageType >::Pointer ValidReader = itk::VideoFileReader< OutputImageType >::New();
  ValidReader->SetFileName(Input);
  ValidReader->UseOpenCV(readerUseOpenCV);

  std::cout << "Done reading file..." << std::endl;

  TestReader->SetNextFrameIsFrameRequested(true);
  TestReader->SetFrameRequested(1);

  std::cout << "Requested next frame..." << std::endl;

  itk::DifferenceImageFilter<OutputImageType,OutputImageType>::Pointer filter
    = itk::DifferenceImageFilter<OutputImageType,OutputImageType>::New();
  filter->SetTestInput(TestReader->GetOutput());
  filter->SetValidInput(ValidReader->GetOutput());
  TestReader->LoadVideo();
  ValidReader->LoadVideo();

  std::cout << "Done loading video..." << std::endl;

  unsigned long i;
  unsigned long FrameTotal = TestReader->GetFrameTotal();

  itk::VideoFileWriter<OutputImageType>::Pointer VideoWriter = itk::VideoFileWriter<OutputImageType>::New();
  VideoWriter->SetInput(filter->GetOutput());
  VideoWriter->SetFileName(Output);
  VideoWriter->UseOpenCV(writerUseOpenCV);

  // Just play the video
  TestReader->SetNextFrameIsFrameRequested(false);

  std::cout << "Reading " << FrameTotal << " frames..." << std::endl;

  for (i = 0; i < FrameTotal-1 ; i ++ )
    {
    try
      {
      VideoWriter->Update();
      }
    catch (itk::ExceptionObject &e)
      {
      std::cerr<<"Error in the loop, step # "<<i<<std::endl;
      std::cerr<<"Total Frames : "<<FrameTotal<<std::endl;
      VideoWriter->Print(std::cout);
      std::cerr<<e.GetFile()<<std::endl;
      std::cerr<<e.GetLine()<<std::endl;
      std::cerr<<e.GetLocation()<<std::endl;
      std::cerr<<e.GetNameOfClass()<<std::endl;
      std::cerr<<e.GetDescription()<<std::endl;
      return EXIT_FAILURE;
      }

    ValidReader->KeepReading();
    TestReader->KeepReading();
    }

  VideoWriter->EndVideo();

  std::cout<<"Done !"<<std::endl;
  return EXIT_SUCCESS;


}

int main (int argv, char **argc)
{
  /*int k = motion_tracker("C:/projects/ITK-Vid-A2D2/Data/inde-circulation.avi", 
    "C:/projects/ITK-Vid-A2D2_build/Testing/Motion_tracked_inde-circulation.avi", 
    atoi("1"),
    atoi("1"));
  std::cin>>k;
  return k;*/

  return motion_tracker(argc[1],argc[2],atoi(argc[3]),atoi(argc[4]));
}
