#ifndef _read_info_cxx
#define _read_info_cxx

/////////////////////////////////
static int readImageInfo(char *filename, itk::ImageIOBase::IOComponentType *ComponentType, int *dim)
{
  itk::ImageIOBase::Pointer imageIO = itk::ImageIOFactory::CreateImageIO(filename, itk::ImageIOFactory::ReadMode);

  if ( imageIO.IsNull() )
    {
    return 0;
    }

  imageIO->SetFileName(filename);
  imageIO->ReadImageInformation();

  *ComponentType = imageIO->GetComponentType();
  *dim = imageIO->GetNumberOfDimensions();
  return ( 1 );
}

/////////////////////////////////
#endif
