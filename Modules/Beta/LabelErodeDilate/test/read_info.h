#ifndef _read_info_h
#define _read_info_h
/////////////////////////////////
int readImageInfo(std::string filename, itk::ImageIOBase::IOComponentType *ComponentType, int *dim);
/////////////////////////////////
#endif
