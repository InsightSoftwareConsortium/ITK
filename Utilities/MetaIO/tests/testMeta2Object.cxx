#include <stdio.h>
#include <fstream>
#include <ctype.h>

#include <metaObject.h>

int main(int argc, char **argv)
  {
  MetaObject tObj;

  tObj.InitializeEssential(3);
  tObj.FileName("testObject.txt");
  tObj.Comment("TestObject");
  tObj.ObjectTypeName("Object");
  tObj.ObjectSubTypeName("MinorObject");
  tObj.Position(0, 1);
  tObj.Position(1, 2);
  tObj.Position(2, 3);
  float orient[9];
  int i;
  for(i=0; i<9; i++)
    {
    orient[i] = 0;
    }
  orient[0] = 1;
  orient[5] = 1;
  orient[7] = 1;
  tObj.Orientation(orient);
  tObj.ElementSpacing(0, 1);
  tObj.ElementSpacing(1, 2);
  tObj.ElementSpacing(2, 1);
  tObj.PrintInfo();
  tObj.Write();
  tObj.Clear();

  tObj.FileName("testObject2.txt");
  tObj.InitializeEssential(2);
  tObj.Position(0, 4);
  tObj.ElementSpacing(0,2);
  tObj.PrintInfo();
  tObj.Write();
  tObj.Clear();

  tObj.Read();
  tObj.PrintInfo();
  if(tObj.NDims() != 2)
    {
    std::cout << "NDims: FAIL" << std::endl;
    }
  else
    {
    std::cout << "NDims: PASS" << std::endl;
    }

  int zero = 0;
  if(tObj.Position(zero) != 4)
    {
    std::cout << "Position: FAIL" << std::endl;
    }
  else
    {
    std::cout << "Position: PASS" << std::endl;
    }
  
  if(tObj.ElementSpacing(zero) != 2)
    {
    std::cout << "ElementSpacing: FAIL" << std::endl;
    }
  else
    {
    std::cout << "ElementSpacing: PASS" << std::endl;
    }

  return 1;
  }
