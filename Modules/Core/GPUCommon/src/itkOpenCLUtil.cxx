/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "itkOpenCLUtil.h"
#include <assert.h>
#include <iostream>
#include <algorithm>

namespace itk
{
//
// Get the block size based on the desired image dimension
//
int OpenCLGetLocalBlockSize(unsigned int ImageDim)
{
  /**
   * OpenCL workgroup (block) size for 1/2/3D - needs to be tuned based on the GPU architecture
   * 1D : 256
   * 2D : 16x16 = 256
   * 3D : 4x4x4 = 64
   */
  int OPENCL_BLOCK_SIZE[3] = { 256, 16, 4 /*8*/ };


  if (ImageDim > 3)
  {
    itkGenericExceptionMacro("Only ImageDimensions up to 3 are supported");
  }
  return OPENCL_BLOCK_SIZE[ImageDim-1];
}

//
// Get the devices that are available.
//
cl_device_id* OpenCLGetAvailableDevices(cl_platform_id platform, cl_device_type devType, cl_uint* numAvailableDevices)
{
  cl_device_id* availableDevices = ITK_NULLPTR;
  cl_uint       totalNumDevices;

  // get total # of devices
  cl_int errid;

  errid = clGetDeviceIDs(platform, devType, 0, ITK_NULLPTR, &totalNumDevices);
  OpenCLCheckError( errid, __FILE__, __LINE__, ITK_LOCATION );

  cl_device_id* totalDevices = (cl_device_id *)malloc(totalNumDevices * sizeof(cl_device_id) );
  errid = clGetDeviceIDs(platform, devType, totalNumDevices, totalDevices, ITK_NULLPTR);
  OpenCLCheckError( errid, __FILE__, __LINE__, ITK_LOCATION );

  (*numAvailableDevices) = 0;

  // check available devices
  for(cl_uint i=0; i<totalNumDevices; i++)
    {
    cl_bool isAvailable;
    clGetDeviceInfo(totalDevices[i], CL_DEVICE_AVAILABLE, sizeof(cl_bool), &isAvailable, ITK_NULLPTR);

    if(isAvailable)
      {
      (*numAvailableDevices)++;
      }
    }

  availableDevices = (cl_device_id *)malloc( (*numAvailableDevices) * sizeof(cl_device_id) );

  int idx = 0;
  for(cl_uint i=0; i<totalNumDevices; i++)
    {
    cl_bool isAvailable;
    clGetDeviceInfo(totalDevices[i], CL_DEVICE_AVAILABLE, sizeof(cl_bool), &isAvailable, ITK_NULLPTR);

    if(isAvailable)
      {
      availableDevices[idx++] = totalDevices[i];
      }
    }

  free( totalDevices );

  return availableDevices;
}

//
// Get the device that has the maximum FLOPS in the current context
//
cl_device_id OpenCLGetMaxFlopsDev(cl_context cxGPUContext)
{
  size_t        szParmDataBytes;
  cl_device_id* cdDevices;

  // get the list of GPU devices associated with context
  clGetContextInfo(cxGPUContext, CL_CONTEXT_DEVICES, 0, ITK_NULLPTR, &szParmDataBytes);
  cdDevices = (cl_device_id*) malloc(szParmDataBytes);
  size_t device_count = szParmDataBytes / sizeof(cl_device_id);

  clGetContextInfo(cxGPUContext, CL_CONTEXT_DEVICES, szParmDataBytes, cdDevices, ITK_NULLPTR);

  cl_device_id max_flops_device = cdDevices[0];

  size_t current_device = 0;

  // CL_DEVICE_MAX_COMPUTE_UNITS
  cl_uint compute_units;
  clGetDeviceInfo(cdDevices[current_device], CL_DEVICE_MAX_COMPUTE_UNITS, sizeof(compute_units), &compute_units, ITK_NULLPTR);

  // CL_DEVICE_MAX_CLOCK_FREQUENCY
  cl_uint clock_frequency;
  clGetDeviceInfo(cdDevices[current_device], CL_DEVICE_MAX_CLOCK_FREQUENCY, sizeof(clock_frequency), &clock_frequency,
                  ITK_NULLPTR);

  int max_flops = compute_units * clock_frequency;
  ++current_device;

  while( current_device < device_count )
    {
    // CL_DEVICE_MAX_COMPUTE_UNITS
    //cl_uint compute_units;
    clGetDeviceInfo(cdDevices[current_device], CL_DEVICE_MAX_COMPUTE_UNITS, sizeof(compute_units), &compute_units, ITK_NULLPTR);

    // CL_DEVICE_MAX_CLOCK_FREQUENCY
    //cl_uint clock_frequency;
    clGetDeviceInfo(cdDevices[current_device], CL_DEVICE_MAX_CLOCK_FREQUENCY, sizeof(clock_frequency), &clock_frequency,
                    ITK_NULLPTR);

    int flops = compute_units * clock_frequency;
    if( flops > max_flops )
      {
      max_flops        = flops;
      max_flops_device = cdDevices[current_device];
      }
    ++current_device;
    }

  free(cdDevices);

  return max_flops_device;
}

//
// Print device name & info
//
void OpenCLPrintDeviceInfo(cl_device_id device, bool verbose)
{
  char device_string[1024];

  clGetDeviceInfo(device, CL_DEVICE_NAME, sizeof(device_string), &device_string, ITK_NULLPTR);
  printf("%s\n", device_string);

  size_t worksize[3];
  clGetDeviceInfo(device,CL_DEVICE_MAX_WORK_ITEM_SIZES,sizeof(worksize),&worksize,ITK_NULLPTR);
  std::cout << "Maximum Work Item Sizes : { " << worksize[0] << ", " << worksize[1] << ", " << worksize[2] << " }" << std::endl;

  size_t maxWorkgroupSize;
  clGetDeviceInfo(device,CL_DEVICE_MAX_WORK_GROUP_SIZE,sizeof(maxWorkgroupSize),&maxWorkgroupSize,ITK_NULLPTR);
  std::cout << "Maximum Work Group Size : " << maxWorkgroupSize << std::endl;

  if (verbose)
  {
    cl_uint mem_align;
    clGetDeviceInfo(device, CL_DEVICE_MEM_BASE_ADDR_ALIGN, sizeof(mem_align), &mem_align, ITK_NULLPTR);
    std::cout << "Alignment in bits of the base address : " << mem_align << std::endl;

    cl_uint min_align;
    clGetDeviceInfo(device, CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE, sizeof(min_align), &min_align, ITK_NULLPTR);
    std::cout << "Smallest alignment in bytes for any data type : " << min_align << std::endl;

    char device_extensions[1024];
    clGetDeviceInfo(device, CL_DEVICE_EXTENSIONS, sizeof(device_extensions), &device_extensions, ITK_NULLPTR);
    printf("%s\n", device_extensions);

  }
}

//
// Find the OpenCL platform that matches the "name"
//
cl_platform_id OpenCLSelectPlatform(const char* name)
{
  char            chBuffer[1024];
  cl_uint         num_platforms;
  cl_platform_id* clPlatformIDs;
  cl_int          ciErrNum;
  cl_platform_id  clSelectedPlatformID = ITK_NULLPTR;

  // Get OpenCL platform count
  ciErrNum = clGetPlatformIDs (0, ITK_NULLPTR, &num_platforms);
  if (ciErrNum != CL_SUCCESS)
    {
    printf(" Error %i in clGetPlatformIDs Call !!!\n\n", ciErrNum);
    }
  else
    {
    if(num_platforms == 0)
      {
      printf("No OpenCL platform found!\n\n");
      }
    else
      {
      // if there's a platform or more, make space for ID's
      if ( (clPlatformIDs = (cl_platform_id*)malloc(num_platforms * sizeof(cl_platform_id) ) ) == ITK_NULLPTR)
        {
        printf("Failed to allocate memory for cl_platform ID's!\n\n");
        }
      else
        {
        ciErrNum = clGetPlatformIDs (num_platforms, clPlatformIDs, ITK_NULLPTR);
        if(ciErrNum == CL_SUCCESS)
          {
          clSelectedPlatformID = clPlatformIDs[0];         // default
  // debug
  ciErrNum = clGetPlatformInfo (clPlatformIDs[0], CL_PLATFORM_NAME, 1024, &chBuffer, ITK_NULLPTR);
  std::cout << "Platform " << " : " << chBuffer << std::endl;
  //
          }

        if(num_platforms > 1)
          {
          std::cout << "Total # of platform : " << num_platforms << std::endl;

          for(cl_uint i = 0; i < num_platforms; ++i)
            {
            ciErrNum = clGetPlatformInfo (clPlatformIDs[i], CL_PLATFORM_NAME, 1024, &chBuffer, ITK_NULLPTR);

// debug
            std::cout << "Platform " << i << " : " << chBuffer << std::endl;
//

            if(ciErrNum == CL_SUCCESS)
              {
              if(strstr(chBuffer, name) != ITK_NULLPTR)
                {
                clSelectedPlatformID = clPlatformIDs[i];
                }
              }
            }
          }
        free(clPlatformIDs);
        }
      }
    }

  return clSelectedPlatformID;
}

void OpenCLCheckError(cl_int error, const char* filename, int lineno, const char* location)
{
  if(error != CL_SUCCESS)
    {
    static const char * const errorString[] = {
      "CL_SUCCESS",
      "CL_DEVICE_NOT_FOUND",
      "CL_DEVICE_NOT_AVAILABLE",
      "CL_COMPILER_NOT_AVAILABLE",
      "CL_MEM_OBJECT_ALLOCATION_FAILURE",
      "CL_OUT_OF_RESOURCES",
      "CL_OUT_OF_HOST_MEMORY",
      "CL_PROFILING_INFO_NOT_AVAILABLE",
      "CL_MEM_COPY_OVERLAP",
      "CL_IMAGE_FORMAT_MISMATCH",
      "CL_IMAGE_FORMAT_NOT_SUPPORTED",
      "CL_BUILD_PROGRAM_FAILURE",
      "CL_MAP_FAILURE",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "CL_INVALID_VALUE",
      "CL_INVALID_DEVICE_TYPE",
      "CL_INVALID_PLATFORM",
      "CL_INVALID_DEVICE",
      "CL_INVALID_CONTEXT",
      "CL_INVALID_QUEUE_PROPERTIES",
      "CL_INVALID_COMMAND_QUEUE",
      "CL_INVALID_HOST_PTR",
      "CL_INVALID_MEM_OBJECT",
      "CL_INVALID_IMAGE_FORMAT_DESCRIPTOR",
      "CL_INVALID_IMAGE_SIZE",
      "CL_INVALID_SAMPLER",
      "CL_INVALID_BINARY",
      "CL_INVALID_BUILD_OPTIONS",
      "CL_INVALID_PROGRAM",
      "CL_INVALID_PROGRAM_EXECUTABLE",
      "CL_INVALID_KERNEL_NAME",
      "CL_INVALID_KERNEL_DEFINITION",
      "CL_INVALID_KERNEL",
      "CL_INVALID_ARG_INDEX",
      "CL_INVALID_ARG_VALUE",
      "CL_INVALID_ARG_SIZE",
      "CL_INVALID_KERNEL_ARGS",
      "CL_INVALID_WORK_DIMENSION",
      "CL_INVALID_WORK_GROUP_SIZE",
      "CL_INVALID_WORK_ITEM_SIZE",
      "CL_INVALID_GLOBAL_OFFSET",
      "CL_INVALID_EVENT_WAIT_LIST",
      "CL_INVALID_EVENT",
      "CL_INVALID_OPERATION",
      "CL_INVALID_GL_OBJECT",
      "CL_INVALID_BUFFER_SIZE",
      "CL_INVALID_MIP_LEVEL",
      "CL_INVALID_GLOBAL_WORK_SIZE",
    };
    // print error message
    const int errorCount = sizeof(errorString) / sizeof(errorString[0]);
    const int index = -error;
    std::ostringstream errorMsg;

    if(index >= 0 && index < errorCount)
    {
      errorMsg << "OpenCL Error : " << errorString[index] << std::endl;
    }
    else
    {
      errorMsg << "OpenCL Error : Unspecified Error" << std::endl;
    }
    ::itk::ExceptionObject e_(filename, lineno, errorMsg.str().c_str(), location);
    throw e_;
    }
}

/** Check if OpenCL-enabled GPU is present. */
bool IsGPUAvailable()
{
  cl_platform_id platformId = OpenCLSelectPlatform("NVIDIA");

  if(platformId == ITK_NULLPTR) return false;

  cl_device_type devType = CL_DEVICE_TYPE_GPU;

  // Get the devices
  cl_uint numDevices;
  cl_device_id* device_id = OpenCLGetAvailableDevices(platformId, devType, &numDevices);
  free( device_id );

  if(numDevices < 1) return false;

  return true;
}

std::string GetTypename(const std::type_info& intype)
{
  std::string typestr;
  if ( intype == typeid ( unsigned char ) ||
       intype == typeid ( itk::Vector< unsigned char, 2 > ) ||
       intype == typeid ( itk::Vector< unsigned char, 3 > ) )
    {
    typestr = "unsigned char";
    }
  else if ( intype == typeid ( char ) ||
            intype == typeid ( itk::Vector< char, 2 > ) ||
            intype == typeid ( itk::Vector< char, 3 > ) )
    {
    typestr = "char";
    }
  else if ( intype == typeid ( short ) ||
            intype == typeid ( itk::Vector< short, 2 > ) ||
            intype == typeid ( itk::Vector< short, 3 > ) )
    {
    typestr = "short";
    }
  else if ( intype == typeid ( int ) ||
            intype == typeid ( itk::Vector< int, 2 > ) ||
            intype == typeid ( itk::Vector< int, 3 > ) )
    {
    typestr = "int";
    }
  else if ( intype == typeid ( unsigned int ) ||
            intype == typeid ( itk::Vector< unsigned int, 2 > ) ||
            intype == typeid ( itk::Vector< unsigned int, 3 > ) )
    {
    typestr = "unsigned int";
    }
  else if ( intype == typeid ( long ) ||
            intype == typeid ( itk::Vector< long, 2 > ) ||
            intype == typeid ( itk::Vector< long, 3 > ) )
    {
    typestr = "long";
    }
  else if ( intype == typeid ( unsigned long ) ||
            intype == typeid ( itk::Vector< unsigned long, 2 > ) ||
            intype == typeid ( itk::Vector< unsigned long, 3 > ) )
    {
    typestr = "unsigned long";
    }
  else if ( intype == typeid ( long long) ||
            intype == typeid ( itk::Vector< long long, 2 > ) ||
            intype == typeid ( itk::Vector< long long, 3 > ) )
    {
    typestr = "long long";
    }
  else if ( intype == typeid ( float ) ||
            intype == typeid ( itk::Vector< float, 2 > ) ||
            intype == typeid ( itk::Vector< float, 3 > ) )
    {
    typestr = "float";
    }
  else if ( intype == typeid ( double ) ||
            intype == typeid ( itk::Vector< double, 2 > ) ||
            intype == typeid ( itk::Vector< double, 3 > ) )
    {
    typestr = "double";
    }
  else
    {
      itkGenericExceptionMacro("Unknown type: " << intype.name());
    }
  return typestr;
}

/** Get Typename in String if a valid type */
bool GetValidTypename(const std::type_info& intype, const std::vector<std::string>& validtypes, std::string& retTypeName)
{
  std::string typestr = GetTypename(intype);
  bool isValid = false;
  std::vector<std::string>::const_iterator validPos;
  validPos = std::find(validtypes.begin(), validtypes.end(), typestr);
  if (validPos != validtypes.end())
    {
      isValid = true;
      retTypeName = *validPos;
    }

  return isValid;
}

/** Get 64-bit pragma */
std::string Get64BitPragma()
{
  std::ostringstream msg;
  msg << "#pragma OPENCL EXTENSION cl_khr_fp64 : enable\n";
  msg << "#pragma OPENCL EXTENSION cl_amd_fp64 : enable\n";
  return msg.str();
}

void GetTypenameInString( const std::type_info& intype, std::ostringstream& ret )
{
  std::string typestr = GetTypename(intype);
  ret << typestr << "\n";
  if ( typestr == "double" )
    {
    std::string pragmastr = Get64BitPragma();
    ret << pragmastr;
    }
}

int GetPixelDimension( const std::type_info& intype )
{
  if ( intype == typeid ( unsigned char ) ||
       intype == typeid ( char ) ||
       intype == typeid ( short ) ||
       intype == typeid ( int ) ||
       intype == typeid ( unsigned int ) ||
       intype == typeid ( float ) ||
       intype == typeid ( double ) )
    {
    return 1;
    }
  else if( intype == typeid ( itk::Vector< unsigned char, 2 > ) ||
           intype == typeid ( itk::Vector< char, 2 > ) ||
           intype == typeid ( itk::Vector< short, 2 > ) ||
           intype == typeid ( itk::Vector< int, 2 > ) ||
           intype == typeid ( itk::Vector< unsigned int, 2 > ) ||
           intype == typeid ( itk::Vector< float, 2 > ) ||
           intype == typeid ( itk::Vector< double, 2 > ) )
    {
    return 2;
    }
  else if( intype == typeid ( itk::Vector< unsigned char, 3 > ) ||
           intype == typeid ( itk::Vector< char, 3 > ) ||
           intype == typeid ( itk::Vector< short, 3 > ) ||
           intype == typeid ( itk::Vector< int, 3 > ) ||
           intype == typeid ( itk::Vector< unsigned int, 3 > ) ||
           intype == typeid ( itk::Vector< float, 3 > ) ||
           intype == typeid ( itk::Vector< double, 3 > ) )
    {
    return 3;
    }
  else
    {
    itkGenericExceptionMacro("Pixeltype is not supported by the filter.");
    }
}

} // end namespace itk
