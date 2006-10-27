#include <stdio.h>
#include <fstream>
#include <ctype.h>

#include <metaForm.h>

int main(int argc, char **argv)
  {
  MetaForm tObj;

  tObj.InitializeEssential();
  tObj.FileName("testForm.txt");
  tObj.Comment("TestForm");
  tObj.FormTypeName("Form");

  // Add user's defined fields
  int myarray[3];
  myarray[0]=1;
  myarray[1]=2;
  myarray[2]=3;
  tObj.AddUserField("MyName", MET_STRING, strlen("Julien"), "Julien");
  tObj.AddUserField("MyArray", MET_INT_ARRAY,3,myarray);

  int i;
  float myMatrix[4];
  for(i=0; i<4; i++)
    {
    myMatrix[i] = (float)i;
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
    return 0;
  }

  int* array = static_cast<int*>(tObj.GetUserField("MyArray"));

  for(i=0;i<3;i++)
  {
    if(array[i] != i+1)
    {
      std::cout << "MyArray: FAIL" << std::endl;
      return 0;
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
  tObj.InitializeEssential();
  tObj.PrintInfo();
  tObj.Write();
  tObj.Clear();

  tObj.Read();
  tObj.PrintInfo();

  return 1;
  }
