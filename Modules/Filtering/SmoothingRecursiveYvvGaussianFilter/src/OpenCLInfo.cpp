// OpenCLInfo.cpp
//
//  Simple code that probes the available platforms and devices
//  using clGetInfo* functions.

#include <iostream>
#include <fstream>
#include <sstream>

#if defined(_WIN32)
#  include <malloc.h> // needed for alloca
#endif                // _WIN32

#if defined(linux) || defined(__APPLE__) || defined(__MACOSX)
#  include <alloca.h>
#endif // linux

#ifdef __APPLE__
#  include <OpenCL/cl.h>
#else
#  include <CL/cl.h>
#endif

///
//  Enumerate platforms and display information about them
//  and their associated devices.
int
main(int argc, char ** argv)
{
  // cl_context context = 0;

  cl_int           errNum;
  cl_uint          numPlatforms;
  cl_platform_id * platformIds;
  cl_context       context = NULL;

  // First, query the total number of platforms
  errNum = clGetPlatformIDs(0, NULL, &numPlatforms);
  if (errNum != CL_SUCCESS || numPlatforms <= 0)
  {
    std::cerr << "Failed to find any OpenCL platform." << std::endl;
    return -1;
  }

  // Next, allocate memory for the installed plaforms, and qeury
  // to get the list.
  platformIds = (cl_platform_id *)alloca(sizeof(cl_platform_id) * numPlatforms);
  // First, query the total number of platforms
  errNum = clGetPlatformIDs(numPlatforms, platformIds, NULL);
  if (errNum != CL_SUCCESS)
  {
    std::cerr << "Failed to find any OpenCL platforms." << std::endl;
    return -1;
  }

  std::cout << "Number of platforms: \t" << numPlatforms << std::endl;
  int result = 1;

  // Iterate through the list of platforms displaying associated information
  for (cl_uint i = 0; i < numPlatforms; i++)
  {

    // Now query the set of devices associated with the platform
    cl_uint numDevices;
    errNum = clGetDeviceIDs(platformIds[i], CL_DEVICE_TYPE_ALL, 0, NULL, &numDevices);
    if (errNum != CL_SUCCESS)
    {
      std::cerr << "Failed to find OpenCL devices." << std::endl;
      return -1;
    }

    cl_device_id * devices = (cl_device_id *)alloca(sizeof(cl_device_id) * numDevices);
    errNum = clGetDeviceIDs(platformIds[i], CL_DEVICE_TYPE_ALL, numDevices, devices, NULL);
    if (errNum != CL_SUCCESS)
    {
      std::cerr << "Failed to find OpenCL devices." << std::endl;
      return -1;
    }

    std::cout << "\tNumber of devices: \t" << numDevices << std::endl;
    // Iterate through each device, displaying associated information

    for (cl_uint j = 0; j < numDevices; j++)
    {
      cl_uint vectorWidthDouble;
      errNum =
        clGetDeviceInfo(devices[j], CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE, sizeof(cl_uint), &vectorWidthDouble, NULL);
      if (vectorWidthDouble == 0)
      {
        std::cout << "Found a device that does not support double precision.\n";
        result = 20;
      }
      cl_ulong sharedMemSize, maxObjMem;
      errNum = clGetDeviceInfo(devices[j], CL_DEVICE_LOCAL_MEM_SIZE, sizeof(cl_ulong), &sharedMemSize, NULL);
      // std::cout<<"sharedMemSize: "<<sharedMemSize/1024<<"KB.\n";
      errNum = clGetDeviceInfo(devices[j], CL_DEVICE_MAX_MEM_ALLOC_SIZE, sizeof(cl_ulong), &maxObjMem, NULL);
      // std::cout<<"MaxObjMem: "<<maxObjMem/(1024.0*1024.0)<<"MB.\n";

      if (maxObjMem / (1024.0 * 1024.0) <= 512 || sharedMemSize <= 49000)
      {
        std::cout << "GPU with little RAM. Setting single-precision.\n";
        result = 10;
      }
    }
  }
  std::cout << "All devices verified.\n";
  return result;
}
