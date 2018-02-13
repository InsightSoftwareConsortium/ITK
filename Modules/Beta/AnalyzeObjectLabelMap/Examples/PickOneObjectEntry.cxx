/* This example is for reading in any file, then creating a
new object map, then add entries to the object map, pick one entry and
then write out the new object map with the one entry only.
*/

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkAnalyzeObjectMap.h"
#include "itkAnalyzeObjectLabelMapImageIOFactory.h"

int main( int argc, char * * argv )
{
  int error_count = 0;

  if( argc != 3 )
    {
    std::cerr << "USAGE: " << argv[0] << "<inputFileName> <outputFileName>" << std::endl;
    }
  const char *NiftiFile = argv[1];
  const char *CreatingObject = argv[2];
  using InputPixelType = unsigned char;
  using OutputPixelType = unsigned char;
  const   unsigned int Dimension = 3;

  using InputImageType = itk::Image<InputPixelType,  Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  // This is very important to use if you are not going to install the Analyze Object map code directly into
  // itk.  This means that you can build the Analyze Object map outside of ITK and still use it and treat
  // the code as if it is in ITK.
  itk::ObjectFactoryBase::RegisterFactory( itk::AnalyzeObjectLabelMapImageIOFactory::New() );

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  // Now we bring in a nifti file that Hans and Jeffrey created, the image as two squares and a circle in it of
  // different intensity values.
  reader->SetFileName(NiftiFile);
  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }
  itk::AnalyzeObjectMap<InputImageType>::Pointer CreateObjectMap = itk::AnalyzeObjectMap<InputImageType>::New();

  // Add another two entries that will be based on the image that is passed into
  // the function, also, the intensity that you would like searched for, the name of the entry and then finally the RGB
  // values
  // you would like the entry to have for the regions that are found.
  CreateObjectMap->AddObjectEntryBasedOnImagePixel(reader->GetOutput(), 200, "Square", 250, 0, 0);
  CreateObjectMap->AddObjectEntryBasedOnImagePixel(reader->GetOutput(), 128, "Circle", 0, 250, 0);
  CreateObjectMap->AddObjectEntryBasedOnImagePixel(reader->GetOutput(), 45,  "SquareTwo", 0, 0, 250);

  // Pick the circle entry and have it put into CreateObjectMap two.  These means
  // that there is only one entry in CreateObjectMapTwo and the image has also
  // been taken care of.
  itk::AnalyzeObjectMap<InputImageType>::Pointer CreateObjectMapTwo = CreateObjectMap->PickOneEntry(2);

  // Now write out an object file
  writer->SetInput(CreateObjectMapTwo);
  writer->SetFileName(CreatingObject);

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}
