
//
//
// NEW APP ARCHITECTURE
// Make a helper class that is instantiated and then told to setup 
// the parser before the parsing begins.  The helper class would 
// register all the callbacks it cares about on the indicated 
// parser.
//
//

#ifdef WIN32
#pragma warning(disable:4786)
#endif

#include <iostream>
#include <vector>

#include "DICOMParser.h"
#include "DICOMCallback.h"
#include "DICOMAppHelper.h"

// #include <stdio.h>
#include <stdlib.h>


int main(int argc, char* argv[])
{

    
  if (argc < 2)
    {
    std::cout << std::endl;
    std::cout << "Usage: DICOMApp file1 file2..." << std::endl;
    std::cout << std::endl;
    std::cout << "==================================================================" << std::endl;
    std::cout << "Image data will be written to file1.raw file2.raw" << std::endl;
    std::cout << "Header values will be written to file1.header.txt file2.header.txt" << std::endl;
    std::cout << "==================================================================" << std::endl;
    std::cout << std::endl;
    std::cout.flush();
    return EXIT_FAILURE;
    }
  
  DICOMParser parser;
  
  DICOMAppHelper* myObj = new DICOMAppHelper;
  
  myObj->RegisterCallbacks(parser);
  
  for (int i = 1; i < argc; i++)
    {
    const char* filename = argv[i];
    
    std::cout << std::endl;
    std::cout << "========== Parsing " << filename << " ===========" << std::endl;
    std::cout << std::endl;
    myObj->SetFileName(filename);
    if (!parser.OpenFile((char*) filename))
      {
      std::cerr << "Couldn't open: " << filename << std::endl;
      continue;
      }
    myObj->SetDICOMDataFile(parser.GetDICOMFile());
    parser.ReadHeader();
    }
  
  myObj->OutputSeries();
  
  return EXIT_SUCCESS;
}
