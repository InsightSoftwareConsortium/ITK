#include <stdio.h>
#include <ctype.h>
#include <metaImage.h>

int testMetaImage(int argc, char *argv[])
  {

  MetaImage tIm(8, 8, 1, 2, MET_CHAR);

  int i;
  for(i=0; i<64; i++)
    tIm.ElementData(i, i);

  for(i=0; i<64; i++)
    {
    if(i != tIm.ElementData(i))
      {
      std::cout << "Assigned Element Values Maintained: FAIL" << std::endl;
      return 0;
      }
    }

  tIm.Write("test.mha");
  tIm.PrintInfo();

  MetaImage tIm2("test.mha");
  tIm2.PrintInfo();
  for(i=0; i<64; i++)
    {
    if(i != tIm.ElementData(i))
      {
      std::cout << "Read Element Values: FAIL" << std::endl;
      return 0;
      }
    }


  return 1;
  }
