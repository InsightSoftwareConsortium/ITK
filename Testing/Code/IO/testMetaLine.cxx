#include <stdio.h>
#include <ctype.h>
#include <metaLine.h>

int testMetaLine(int , char * [])
{

  std::cout << "Creating test file ...";
  MetaLine line0;
  MetaLine* Line = new MetaLine(3);
  Line->ID(0);
  LinePnt* pnt;

  unsigned int i;
  for(i=0;i<10;i++)
  {
    pnt = new LinePnt(3);
    pnt->m_X[0]=(float)0.2;pnt->m_X[1]=i;pnt->m_X[2]=i;
    pnt->m_V[0][0]=(float)0.3;pnt->m_V[0][1]=i;pnt->m_V[0][2]=i;
    pnt->m_V[1][0]=(float)0.4;pnt->m_V[1][1]=i+1;pnt->m_V[1][2]=i+1;
    Line->GetPoints().push_back(pnt);
  }
  
  std::cout << "Writing test file ...";
   
  Line->BinaryData(true);

  Line->Write("myLine.meta");

  std::cout << "done" << std::endl;
  std::cout << "Reading test file ...";

  Line->Clear();
  Line->Read("myLine.meta");

  MetaLine LineRead("myLine.meta");
  MetaLine LineCopy(&LineRead);

  std::cout << "PointDim = " << LineCopy.PointDim() << std::endl;
  std::cout << "NPoints = " << LineCopy.NPoints() << std::endl;
  std::cout << "ElementType = " << LineCopy.ElementType() << std::endl;

  Line->PrintInfo();

  MetaLine::PointListType list =  Line->GetPoints();
  MetaLine::PointListType::const_iterator it = list.begin();
  
  i=0;
  while(it != list.end())
  {
    std::cout << "Point #" << i++ << ":" << std::endl;
    std::cout << "position = ";
    unsigned int d;
    for(d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_X[d] << " ";
    }
    std::cout << std::endl;
    std::cout << "First normal = ";
    for(d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_V[0][d] << " ";
    }
    std::cout << std::endl;
    std::cout << "Second normal = ";
    for(d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_V[1][d] << " ";
    }
    std::cout << std::endl;
    it++;
  }

  delete Line;
  std::cout << "done" << std::endl;
  return 0;
}
