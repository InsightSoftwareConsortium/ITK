//
// Created by Hans J. Johnson on 11/4/18.
//

#include <iostream>
#include <stdlib.h>
#include "vnl_drand48.h"

int main(int, char*[])
{
  for( long seedvalue = 0; seedvalue < 10000; ++seedvalue )
  {
    srand48(seedvalue);
    const double stdlib_value = drand48();
    vnl_srand48(seedvalue);
    const double vxl_value = vnl_drand48();
    if ( stdlib_value != vxl_value )
    {
      std::cout << "FAILURE: ";
    }
    std::cout << stdlib_value << " = " << vxl_value << std::endl;
  }
  return EXIT_SUCCESS;
}
