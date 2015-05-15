#ifndef _read_info_h
#define _read_info_h
/////////////////////////////////
#include "itkImageFileReader.h"

int readImageInfo(char *filename, itk::ImageIOBase::IOComponentType *ComponentType, int *dim);

/////////////////////////////////
#endif
