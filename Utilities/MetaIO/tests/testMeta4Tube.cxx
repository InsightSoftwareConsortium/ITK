#include <cstdio>
#include <ctype.h>
#include <metaTube.h>
#include <metaScene.h>
#include <metaEllipse.h>

int main(int argc, char **argv)
{

  MetaScene myScene = MetaScene(3);

  std::cout << "Creating test file ...";

  //MetaTubeNet* tubenet = new MetaTubeNet();
  
  // add two tube to the list of tubenet
  MetaTube* tube1 = new MetaTube(3);
  tube1->ID(0);
  TubePnt* pnt;

  unsigned int i;
  for(i=0;i<10;i++)
  {
    pnt = new TubePnt(3);
    pnt->m_X[0]=i;pnt->m_X[1]=i;pnt->m_X[2]=i;
    pnt->m_R=i;
    tube1->GetPoints()->push_back(pnt);
  }
  
  MetaTube* tube2 = new MetaTube(3);
  tube2->ID(1);
  for(i=0;i<5;i++)
  {
    pnt = new TubePnt(3);
    pnt->m_X[0]=i;pnt->m_X[1]=i;pnt->m_X[2]=i;
    pnt->m_R=i;
    tube2->GetPoints()->push_back(pnt);
  }

  // Add an ellipse
  MetaEllipse* ellipse = new MetaEllipse();
  ellipse->InitializeEssential(3);
  ellipse->Radius(1,2,3);

  myScene.AddObject(tube1);
  myScene.AddObject(tube2);
  myScene.AddObject(ellipse);
  myScene.Write("test.tre");

  std::cout << "done" << std::endl;
  std::cout << "Reading test file ...";

  // Read the result 
  MetaScene myScene2 = MetaScene();
  myScene2.InitializeEssential(3);
 
  myScene2.Read("test.tre");
  
  typedef  MetaScene::ObjectListType ListType;
  ListType list = myScene2.GetObjectList();
  ListType::iterator it = list.begin();

  for(i=0;i< list.size();i++)
  {
    
    (*it)->PrintInfo();
    if(!strncmp((*it)->ObjectTypeName(),"Tube",4))
    {
      typedef MetaTube::PointListType ListType;
      MetaTube* tube = dynamic_cast<MetaTube*>(*it);
      ListType::iterator it2 = tube->GetPoints()->begin();

      for(unsigned int j=0;j< tube->GetPoints()->size();j++)
      {
        std::cout << (*it2)->m_X[0] 
        << " " << (*it2)->m_X[1] << " " << (*it2)->m_X[2] << std::endl;
        it2++;
      }
    }

    it++;
  }

  std::cout << "done" << std::endl;
  return 1;
}
