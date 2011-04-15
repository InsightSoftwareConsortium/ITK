#include <iostream>

//DEBUG
#include "itkTemporalProcessObject.h"

/**
 * Test functionality of itkTemporalProcessObject
 */
int itkTemporalProcessObjectTest ( int argc, char *argv[] )
{

  itk::Object::Pointer po = itk::TemporalProcessObject::New();

  return EXIT_SUCCESS;
}
