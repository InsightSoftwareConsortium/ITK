#include <stdio.h>
#include <ctype.h>
#include <metaLandmark.h>

int main(int , char * [])
{

  std::cout << "Creating test file ..." << std::endl;
  MetaLandmark Landmark(3);
  Landmark.ID(0);
  LandmarkPnt* pnt;

  std::cout << "Allocating points..." << std::endl;
  unsigned int i;
  for(i=0;i<10;i++)
  {
    pnt = new LandmarkPnt(3);
    pnt->m_X[0]=(float)0.2;pnt->m_X[1]=i;pnt->m_X[2]=i;
    Landmark.GetPoints().push_back(pnt);
  }
  
  std::cout << "Writing test file ..." << std::endl;
   
  Landmark.BinaryData(true);
  Landmark.ElementType(MET_FLOAT);
  Landmark.Write("Landmarks.meta");

  std::cout << "  done" << std::endl;
 
  std::cout << "Reading test file ..." << std::endl;
  Landmark.Read("Landmarks.meta"); 

  std::cout << "  done" << std::endl;

  Landmark.PrintInfo();

  std::cout << "Accessing pointlist..." << std::endl;

  MetaLandmark::PointListType plist =  Landmark.GetPoints();
  MetaLandmark::PointListType::const_iterator it = plist.begin();
  
  while(it != plist.end())
  {
    for(unsigned int d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_X[d] << " ";
    }

    std::cout << std::endl;
    it++;
  }

  std::cout << "done" << std::endl;
  return 0;
}
