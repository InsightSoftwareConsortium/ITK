// This is core/vnl/vnl_block.cxx
//:
// \file
// \author fsm

#include <iostream>
#include <cassert>

void
vnl_block_raise_exception(const char * FILE, int LINE, const char * why)
{
  std::cerr << FILE << ":" << LINE << ": " << why << std::endl;
  assert(!"raise_exeption() called");
  // throw;
}
