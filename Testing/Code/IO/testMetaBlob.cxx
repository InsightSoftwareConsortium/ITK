#include <stdio.h>
#include <ctype.h>
#include <metaBlob.h>

int testMetaBlob(int , char * [])
{

  std::cout << "Creating test file ..." << std::endl;
  MetaBlob blob(3);
  blob.ID(0);
  BlobPnt* pnt;

  std::cout << "Allocating points..." << std::endl;
  unsigned int i;
  for(i=0;i<10;i++)
  {
    pnt = new BlobPnt(3);
    pnt->m_X[0]=(float)0.2;pnt->m_X[1]=i;pnt->m_X[2]=i;
    blob.GetPoints().push_back(pnt);
  }
  
  std::cout << "Writing test file ..." << std::endl;
   
  blob.BinaryData(true);
  blob.ElementType(MET_FLOAT);
  blob.Write("myCNC.meta");

  std::cout << "  done" << std::endl;
 
  std::cout << "Reading test file ..." << std::endl;
  blob.Read("myCNC.meta"); 

  std::cout << "  done" << std::endl;

  blob.PrintInfo();

  std::cout << "Accessing pointlist..." << std::endl;

  MetaBlob::PointListType plist =  blob.GetPoints();
  MetaBlob::PointListType::const_iterator it = plist.begin();
  
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
