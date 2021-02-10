#include <iostream>

#include <metaTube.h>
#include <metaScene.h>
#include <metaEllipse.h>
#include <metaVesselTube.h>

int
main(int, char *[])
{
  std::cout << "Initializing scene ..." << std::endl;
  MetaScene myScene = MetaScene(3);

  std::cout << "Creating test file ..." << std::endl;

  // MetaTubeNet* tubenet = new MetaTubeNet();

  // add two tube to the list of tubenet
  std::cout << "  Creating first tube ..." << std::endl;
  auto * tube1 = new MetaTube(3);
  tube1->ID(0);
  TubePnt * pnt;

  unsigned int i;
  for (i = 0; i < 10; i++)
  {
    pnt = new TubePnt(3);
    pnt->m_X[0] = i;
    pnt->m_X[1] = i;
    pnt->m_X[2] = i;
    pnt->m_R = i;
    tube1->GetPoints().push_back(pnt);
  }

  std::cout << "  Creating second tube ..." << std::endl;
  auto * tube2 = new MetaTube(3);
  tube2->ID(1);
  for (i = 0; i < 5; i++)
  {
    pnt = new TubePnt(3);
    pnt->m_X[0] = i;
    pnt->m_X[1] = i;
    pnt->m_X[2] = i;
    pnt->m_R = i;
    tube2->GetPoints().push_back(pnt);
  }

  // Add an ellipse
  std::cout << "  Creating ellipse ..." << std::endl;
  auto * ellipse = new MetaEllipse();
  std::cout << "    Initializing ellipse ..." << std::endl;
  ellipse->InitializeEssential(3);
  std::cout << "    Setting radius ..." << std::endl;
  ellipse->Radius(1, 2, 3);

  myScene.AddObject(tube1);
  myScene.AddObject(tube2);
  myScene.AddObject(ellipse);

  myScene.Write("test.scn");

  std::cout << "done" << std::endl;
  std::cout << "Reading test file ..." << std::endl;

  // Read the result
  MetaScene myScene2 = MetaScene();
  myScene2.InitializeEssential(3);

  std::cout << "  ... reading scene " << std::endl;
  myScene2.Read("test.scn");
  std::cout << "  ... read scene " << std::endl;

  using ListType = MetaScene::ObjectListType;
  ListType *         list = myScene2.GetObjectList();
  auto it = list->begin();

  std::cout << "  ... beginning loop " << std::endl;
  for (i = 0; i < list->size(); i++)
  {

    (*it)->PrintInfo();
    if (!strncmp((*it)->ObjectTypeName(), "Tube", 4))
    {
      auto *                      tube = dynamic_cast<MetaTube *>(*it);
      auto              it2 = tube->GetPoints().begin();

      for (unsigned int j = 0; j < tube->GetPoints().size(); j++)
      {
        std::cout << (*it2)->m_X[0] << " " << (*it2)->m_X[1] << " " << (*it2)->m_X[2] << std::endl;
        ++it2;
      }
    }

    ++it;
  }

  //Exercise the initialization
  auto silly_instantiation = VesselTubePnt(6);
  constexpr float sentinal_value = 1.234567F;
  silly_instantiation.SetField("SomeField", sentinal_value);
  if(silly_instantiation.GetField("SomeField") != sentinal_value)
  {
    std::cout << "ERROR: Set/Get Field round trip failed." << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "done" << std::endl;
  return EXIT_SUCCESS;
}
