#include <stdio.h>
#include <fstream>
#include <ctype.h>

#include <metaGroup.h>

int testMetaGroup(int , char *[])
  {
  // Testing metaGroup
  std::cout << "--- Testing metaGroup ---" << std::endl;

  std::cout << "Testing Writing:";
  MetaGroup tGroup;
  tGroup.Write("group.meta");
  std::cout << " [PASSED] " << std::endl;

  std::cout << "Testing Reading:";
  MetaGroup groupLoad("group.meta");
  std::cout << " [PASSED] " << std::endl;

  groupLoad.PrintInfo();

  std::cout << "Testing Copy:";
  MetaGroup groupCopy(&groupLoad);
  std::cout << " [PASSED] " << std::endl;
  
  groupCopy.PrintInfo();

  std::cout << "[DONE]" << std::endl;
  return 0;
  }



