#include <stdio.h>
#include <fstream>
#include <ctype.h>

#include <metaObject.h>

int testMetaObject(int argc, char *argv[])
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

  // Add user's defined fields
  int myarray[3];
  myarray[0]=1;
  myarray[1]=2;
  myarray[2]=3;
  tObj.AddUserField("MyName", MET_STRING, strlen("Julien"), "Julien");
  tObj.AddUserField("MyArray", MET_INT_ARRAY,3,myarray);

  float myMatrix[4];
  for(i=0; i<4; i++)
    {
    myMatrix[i] = i;
    } 
  tObj.AddUserField("MyMatrix", MET_FLOAT_MATRIX,2,myMatrix);

  tObj.PrintInfo();
  tObj.Write();

  tObj.Clear();
  tObj.ClearUserFields();
  
  tObj.AddUserField("MyName", MET_STRING);
  tObj.AddUserField("MyArray", MET_INT_ARRAY,3);
  tObj.AddUserField("MyMatrix", MET_FLOAT_MATRIX,2);

  tObj.Read();
  tObj.PrintInfo();

  char* name = static_cast<char*>(tObj.GetUserField("MyName"));
  if(strcmp(name,"Julien"))
  {
    std::cout << "MyName: FAIL" << std::endl;
    return 1;
  }

  int* array = static_cast<int*>(tObj.GetUserField("MyArray"));

  for(i=0;i<3;i++)
  {
    if(array[i] != i+1)
    {
      std::cout << "MyArray: FAIL" << std::endl;
      return 1;
    }
  }

  float* matrix = static_cast<float*>(tObj.GetUserField("MyMatrix"));
  for(i=0; i<4; i++)
  {
    if(matrix[i] != i)
    {
      std::cout << "MyMatrix: FAIL" << std::endl;
    }
  } 

  std::cout << "PASSED!" << std::endl;

  tObj.Clear();
  tObj.ClearUserFields();

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
    std::cout << "Position: FAIL :" << tObj.Position(zero) << std::endl;
    }
  else
    {
    std::cout << "Position: PASS" << std::endl;
    }
  
  if(tObj.ElementSpacing(zero) != 2)
    {
    std::cout << "ElementSpacing: FAIL: " << tObj.ElementSpacing(zero) << std::endl;
    }
  else
    {
    std::cout << "ElementSpacing: PASS" << std::endl;
    }

  return 0;
  }
