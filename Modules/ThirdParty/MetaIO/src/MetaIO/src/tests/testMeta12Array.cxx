#include <iostream>
#include <cstdlib>

#include <metaArray.h>

int main(int, char * [])
{
  MetaArray tObj;

  tObj.InitializeEssential(10, MET_DOUBLE, 1, nullptr, true);
  tObj.FileName("testArray.mvh");
  tObj.Comment("TestArray");
  tObj.FormTypeName("Array");

  tObj.Length(10);
  tObj.ElementType(MET_DOUBLE);
  tObj.BinaryData(false);

  int i;
  for(i=0; i<10; i++)
    {
    tObj.ElementData(i, i*3.1415926);
    }
  tObj.PrintInfo();
  tObj.Write();

  tObj.BinaryData(true);
  tObj.FileName("testArray.mva");
  tObj.PrintInfo();
  tObj.Write();

  MetaArray tObj_ascii, tObj_binary;
  tObj_ascii.FileName("testArray.mvh");
  tObj_binary.FileName("testArray.mva");

  tObj_ascii.Read(nullptr, true, nullptr, true);
  tObj_binary.Read(nullptr, true, nullptr, true);

  tObj_ascii.PrintInfo();
  tObj_binary.PrintInfo();

  std::cout << "PASSED!" << std::endl;

  return EXIT_SUCCESS;
}
