#ifndef _read_info_cxx
#define _read_info_cxx
#include "read_info.h"

/////////////////////////////////
int readImageInfo(std::string filename, itk::ImageIOBase::IOComponentType *ComponentType, int *dim)
{
  itk::ImageIOBase::Pointer imageIO = itk::ImageIOFactory::CreateImageIO(filename.c_str(), itk::ImageIOFactory::ReadMode);
  if (imageIO.IsNull())
    return 0;


  imageIO->SetFileName(filename.c_str());
  imageIO->ReadImageInformation();

  *ComponentType = imageIO->GetComponentType();
  *dim = imageIO->GetNumberOfDimensions();
  return(1);
}
/////////////////////////////////
#endif
