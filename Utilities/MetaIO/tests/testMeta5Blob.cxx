#include <cstdio>
#include <ctype.h>
#include <metaBlob.h>

int main(int argc, char **argv)
{

  std::cout << "Creating test file ...";
  /*MetaBlob* blob = new MetaBlob(3);
  blob->ID(0);
  BlobPnt* pnt;

  unsigned int i;
  for(i=0;i<10;i++)
  {
    pnt = new BlobPnt(3);
    pnt->m_X[0]=(float)0.2;pnt->m_X[1]=i;pnt->m_X[2]=i;
    blob->GetPoints()->push_back(pnt);
  }
  
  std::cout << "Writing test file ...";
   
  blob->BinaryData(true);
  blob->ElementType(MET_USHORT);
  blob->Write("myCNC.meta");

  std::cout << "done" << std::endl;
 
  std::cout << "Reading test file ...";
 */
  /*
  blob->Clear();
  blob->Read("myCNC.meta"); 
  */
  
  //blob->Clear();
  MetaBlob* blob = new MetaBlob;
  blob->Read("liver032c.meta");
  //blob->Read("myCNC.meta");  

  blob->PrintInfo();

  MetaBlob::PointListType* list =  blob->GetPoints();
  MetaBlob::PointListType::const_iterator it = list->begin();
  
  while(it != list->end())
  {
    for(unsigned int d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_X[d] << " ";
    }

    std::cout << std::endl;
    it++;
  }

  std::cout << "done" << std::endl;
  return 1;
}
