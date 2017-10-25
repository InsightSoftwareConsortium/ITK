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

#include "itkGPUKernelManager.h"

namespace itk
{
GPUKernelManager::GPUKernelManager()
{
  m_Program = ITK_NULLPTR;
  m_Manager = GPUContextManager::GetInstance();

  if(m_Manager->GetNumberOfCommandQueues() > 0) m_CommandQueueId = 0;   // default
                                                                  // command
                                                                  // queue
}

GPUKernelManager::~GPUKernelManager()
{
  cl_int errid;

  while(m_KernelContainer.size() > 0)
    {
    errid = clReleaseKernel(m_KernelContainer.back());
    OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);
    m_KernelContainer.pop_back();
    }

  if(m_Program != ITK_NULLPTR)
    {
    errid = clReleaseProgram(m_Program);
    OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);
    }
}

bool GPUKernelManager::LoadProgramFromFile(const char* filename, const char* cPreamble)
{
  // locals
  FILE*  pFileStream = ITK_NULLPTR;
  size_t szSourceLength;
  size_t szFinalLength;

  // open the OpenCL source code file
#ifdef _WIN32   // Windows version
  if(fopen_s(&pFileStream, filename, "rb") != 0)
    {
    itkWarningMacro("Cannot open OpenCL source file");
    return false;
    }
#else           // Linux version
  // printout OpenCL source Path
  std::cout << "Loading source file: " << filename << std::endl;
  pFileStream = fopen(filename, "rb");
  if(pFileStream == ITK_NULLPTR)
    {
    itkWarningMacro("Cannot open OpenCL source file");
    return false;
    }
#endif

  size_t szPreambleLength = strlen(cPreamble);

  // get the length of the source code
  fseek(pFileStream, 0, SEEK_END);
  szSourceLength = ftell(pFileStream);
  fseek(pFileStream, 0, SEEK_SET);

  // allocate a buffer for the source code string and read it in
  char* cSourceString = (char *)malloc(szSourceLength + szPreambleLength + 1000);
  if(szPreambleLength > 0) memcpy(cSourceString, cPreamble, szPreambleLength);
  if (fread( (cSourceString) + szPreambleLength, szSourceLength, 1, pFileStream) != 1)
    {
    fclose(pFileStream);
    free(cSourceString);
    return false;
    }

  // close the file and return the total length of the combined (preamble +
  // source) string
  fclose(pFileStream);

  szFinalLength = szSourceLength + szPreambleLength;

  cSourceString[szSourceLength + szPreambleLength] = '\0';

  //
  // Create OpenCL program from source strings
  //
  cl_int errid;
  m_Program = clCreateProgramWithSource(
      m_Manager->GetCurrentContext(), 1, const_cast<const char **>(&cSourceString), &szFinalLength, &errid);
  OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);
  free(cSourceString);

  if(errid != CL_SUCCESS)
    {
    itkWarningMacro("Cannot create GPU program");
    return false;
    }

  // build program
  errid = clBuildProgram(m_Program, 0, ITK_NULLPTR, ITK_NULLPTR, ITK_NULLPTR, ITK_NULLPTR);
  if(errid != CL_SUCCESS)
    {
    //itkWarningMacro("OpenCL program build error");

    // print out build error
    size_t paramValueSize = 0;

    // get error message size
    clGetProgramBuildInfo(m_Program, m_Manager->GetDeviceId(0), CL_PROGRAM_BUILD_LOG, 0, ITK_NULLPTR, &paramValueSize);

    char *paramValue;
    paramValue = (char*)malloc(paramValueSize);

    // get error message
    clGetProgramBuildInfo(m_Program, m_Manager->GetDeviceId(0), CL_PROGRAM_BUILD_LOG, paramValueSize, paramValue, ITK_NULLPTR);

    /*
    std::ostringstream itkmsg;
    itkmsg << "ERROR: In " __FILE__ ", line " << __LINE__ << "\n"
           << this->GetNameOfClass() << " (" << this << "): "
           << "OpenCL program build error:" << paramValue
           << "\n\n";
    ::itk::OutputWindowDisplayErrorText( itkmsg.str().c_str() );
    */

    std::cerr << paramValue << std::endl;

    free( paramValue );

    OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);

    return false;
    }

  return true;
}

bool GPUKernelManager::LoadProgramFromString(const char* cSource, const char* cPreamble)
{
  size_t szSourceLength;
  size_t szPreambleLength;
  size_t szFinalLength;

  szSourceLength = strlen(cSource);
  szPreambleLength = strlen(cPreamble);
  szFinalLength = szSourceLength + szPreambleLength;

  // allocate a buffer for the source code string and read it in
  char* cSourceString = (char *)malloc(szFinalLength + 1);
  if(szPreambleLength > 0) memcpy(cSourceString, cPreamble, szPreambleLength);

  memcpy(cSourceString + szPreambleLength, cSource, szSourceLength);


  cSourceString[szFinalLength] = '\0';

  //
  // Create OpenCL program from source strings
  //
  cl_int errid;
  m_Program = clCreateProgramWithSource(
      m_Manager->GetCurrentContext(), 1, const_cast<const char **>(&cSourceString), &szFinalLength, &errid);
  OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);
  free(cSourceString);

  if(errid != CL_SUCCESS)
    {
    itkWarningMacro("Cannot create GPU program");
    return false;
    }

  // build program
  errid = clBuildProgram(m_Program, 0, ITK_NULLPTR, ITK_NULLPTR, ITK_NULLPTR, ITK_NULLPTR);
  if(errid != CL_SUCCESS)
    {
    //itkWarningMacro("OpenCL program build error");

    // print out build error
    size_t paramValueSize = 0;

    // get error message size
    clGetProgramBuildInfo(m_Program, m_Manager->GetDeviceId(0), CL_PROGRAM_BUILD_LOG, 0, ITK_NULLPTR, &paramValueSize);

    char *paramValue;
    paramValue = (char*)malloc(paramValueSize);

    // get error message
    clGetProgramBuildInfo(m_Program, m_Manager->GetDeviceId(0), CL_PROGRAM_BUILD_LOG, paramValueSize, paramValue, ITK_NULLPTR);

    /*
    std::ostringstream itkmsg;
    itkmsg << "ERROR: In " __FILE__ ", line " << __LINE__ << "\n"
           << this->GetNameOfClass() << " (" << this << "): "
           << "OpenCL program build error:" << paramValue
           << "\n\n";
    ::itk::OutputWindowDisplayErrorText( itkmsg.str().c_str() );
    */

    std::cerr << paramValue << std::endl;

    free( paramValue );

    OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);

    return false;
    }

  return true;
}

int GPUKernelManager::CreateKernel(const char* kernelName)
{
  cl_int errid;

  // create kernel
  cl_kernel newKernel = clCreateKernel(m_Program, kernelName, &errid);

  OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);

  if(errid != CL_SUCCESS)   // failed
    {
    itkWarningMacro("Fail to create GPU kernel");
    return -1;
    }

  m_KernelContainer.push_back( newKernel );

  // argument list
  m_KernelArgumentReady.push_back( std::vector< KernelArgumentList >() );
  cl_uint nArg;
  errid = clGetKernelInfo( newKernel, CL_KERNEL_NUM_ARGS, sizeof(cl_uint), &nArg, ITK_NULLPTR);
  (m_KernelArgumentReady.back() ).resize( nArg );

  ResetArguments( (int)m_KernelContainer.size()-1 );

  return (int)m_KernelContainer.size()-1;
}

cl_int GPUKernelManager::GetKernelWorkGroupInfo(int kernelIdx,
                                                cl_kernel_work_group_info paramName, void *value)
{
  size_t valueSize, valueSizeRet;

  switch (paramName)
    {
    case CL_KERNEL_WORK_GROUP_SIZE:
      valueSize = sizeof(size_t);
      break;
    case CL_KERNEL_COMPILE_WORK_GROUP_SIZE:
      valueSize = 3 * sizeof(size_t);
      break;
    case CL_KERNEL_LOCAL_MEM_SIZE:
      valueSize = sizeof(cl_ulong);
      break;
    default:
      itkGenericExceptionMacro (<< "Unknown type of work goup information");
    }

  cl_int errid = clGetKernelWorkGroupInfo(m_KernelContainer[kernelIdx], m_Manager->GetDeviceId(0),
                                          paramName, valueSize, value, &valueSizeRet);

  OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);

  return errid;
}

cl_int GPUKernelManager::GetDeviceInfo(
                     cl_kernel_work_group_info paramName,
                     size_t argSize, void *argValue)
{
  cl_int errid;

  switch (paramName)
    {
    case CL_DEVICE_MAX_WORK_ITEM_SIZES:
      errid = clGetDeviceInfo(m_Manager->GetDeviceId(0),
        CL_DEVICE_MAX_WORK_ITEM_SIZES, argSize, argValue, ITK_NULLPTR);
      break;
    default:
      itkGenericExceptionMacro (<< "Unknown type of device info");
    }
  OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);

  return errid;
}

bool GPUKernelManager::SetKernelArg(int kernelIdx, cl_uint argIdx, size_t argSize, const void* argVal)
{
  if(kernelIdx < 0 || kernelIdx >= (int)m_KernelContainer.size() ) return false;

  cl_int errid;

  errid = clSetKernelArg(m_KernelContainer[kernelIdx], argIdx, argSize, argVal);
  OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);

  m_KernelArgumentReady[kernelIdx][argIdx].m_IsReady = true;
  m_KernelArgumentReady[kernelIdx][argIdx].m_GPUDataManager = (GPUDataManager::Pointer)ITK_NULLPTR;

  return true;
}

bool GPUKernelManager::SetKernelArgWithImage(int kernelIdx, cl_uint argIdx, GPUDataManager::Pointer manager)
{
  if(kernelIdx < 0 || kernelIdx >= (int)m_KernelContainer.size() ) return false;

  cl_int errid;

  errid = clSetKernelArg(m_KernelContainer[kernelIdx], argIdx, sizeof(cl_mem), manager->GetGPUBufferPointer() );
  OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);

  m_KernelArgumentReady[kernelIdx][argIdx].m_IsReady = true;
  m_KernelArgumentReady[kernelIdx][argIdx].m_GPUDataManager = manager;

  return true;
}

// this function must be called right before GPU kernel is launched
bool GPUKernelManager::CheckArgumentReady(int kernelIdx)
{
  int nArg = m_KernelArgumentReady[kernelIdx].size();

  for(int i=0; i<nArg; i++)
    {
    if(!(m_KernelArgumentReady[kernelIdx][i].m_IsReady) ) return false;

    // automatic synchronization before kernel launch
    if(m_KernelArgumentReady[kernelIdx][i].m_GPUDataManager != (GPUDataManager::Pointer)ITK_NULLPTR)
      {
      m_KernelArgumentReady[kernelIdx][i].m_GPUDataManager->SetCPUBufferDirty();
      }
    }
  return true;
}

void GPUKernelManager::ResetArguments(int kernelIdx)
{
  int nArg = m_KernelArgumentReady[kernelIdx].size();

  for(int i=0; i<nArg; i++)
    {
    m_KernelArgumentReady[kernelIdx][i].m_IsReady = false;
    m_KernelArgumentReady[kernelIdx][i].m_GPUDataManager = (GPUDataManager::Pointer)ITK_NULLPTR;
    }
}

bool GPUKernelManager::LaunchKernel1D(int kernelIdx, size_t globalWorkSize, size_t itkNotUsed(localWorkSize))
{
  if(kernelIdx < 0 || kernelIdx >= (int)m_KernelContainer.size() ) return false;

  if(!CheckArgumentReady(kernelIdx) )
    {
    itkWarningMacro("GPU kernel arguments are not completely assigned");
    return false;
    }

  cl_int errid;
  // TODO should we allow the user to determine localWorkSize?
//   errid = clEnqueueNDRangeKernel(m_Manager->GetCommandQueue(
//                                    m_CommandQueueId), m_KernelContainer[kernelIdx], 1, ITK_NULLPTR, &globalWorkSize,
//                                  &localWorkSize, 0, ITK_NULLPTR, ITK_NULLPTR);
  errid = clEnqueueNDRangeKernel(m_Manager->GetCommandQueue(
                                   m_CommandQueueId), m_KernelContainer[kernelIdx], 1, ITK_NULLPTR, &globalWorkSize,
                                 ITK_NULLPTR, 0, ITK_NULLPTR, ITK_NULLPTR);
  OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);

  if(errid != CL_SUCCESS)
    {
    itkWarningMacro("GPU kernel launch failed");
    return false;
    }

  return true;
}

bool GPUKernelManager::LaunchKernel2D(int kernelIdx,
                                      size_t globalWorkSizeX, size_t globalWorkSizeY,
                                      size_t itkNotUsed(localWorkSizeX),  size_t itkNotUsed(localWorkSizeY) )
{
  if(kernelIdx < 0 || kernelIdx >= (int)m_KernelContainer.size() ) return false;

  if(!CheckArgumentReady(kernelIdx) )
    {
    itkWarningMacro("GPU kernel arguments are not completely assigned");
    return false;
    }

  size_t gws[2];
  gws[0] = globalWorkSizeX;
  gws[1] = globalWorkSizeY;

//  size_t lws[2];
//  lws[0] = localWorkSizeX;
//  lws[1] = localWorkSizeY;

  cl_int errid;
  // TODO should we allow the user to determine localWorkSize?
//   errid = clEnqueueNDRangeKernel(m_Manager->GetCommandQueue(
//                                    m_CommandQueueId), m_KernelContainer[kernelIdx], 2, ITK_NULLPTR, gws,
//                                  lws, 0, ITK_NULLPTR, ITK_NULLPTR);
  errid = clEnqueueNDRangeKernel(m_Manager->GetCommandQueue(
                                   m_CommandQueueId), m_KernelContainer[kernelIdx], 2, ITK_NULLPTR, gws, ITK_NULLPTR, 0, ITK_NULLPTR, ITK_NULLPTR);
  OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);

  if(errid != CL_SUCCESS)
    {
    itkWarningMacro("GPU kernel launch failed");
    return false;
    }

  return true;
}

bool GPUKernelManager::LaunchKernel3D(int kernelIdx,
                                      size_t globalWorkSizeX, size_t globalWorkSizeY, size_t globalWorkSizeZ,
                                      size_t itkNotUsed(localWorkSizeX),  size_t itkNotUsed(localWorkSizeY), size_t itkNotUsed(localWorkSizeZ) )
{
  if(kernelIdx < 0 || kernelIdx >= (int)m_KernelContainer.size() ) return false;

  if(!CheckArgumentReady(kernelIdx) )
    {
    itkWarningMacro("GPU kernel arguments are not completely assigned");
    return false;
    }

  size_t gws[3];
  gws[0] = globalWorkSizeX;
  gws[1] = globalWorkSizeY;
  gws[2] = globalWorkSizeZ;

//  size_t lws[3];
//  lws[0] = localWorkSizeX;
//  lws[1] = localWorkSizeY;
//  lws[2] = localWorkSizeZ;

  cl_int errid;
  // TODO should we allow the user to determine localWorkSize?
//   errid = clEnqueueNDRangeKernel(m_Manager->GetCommandQueue(
//                                    m_CommandQueueId), m_KernelContainer[kernelIdx], 3, ITK_NULLPTR, gws, lws, 0, ITK_NULLPTR, ITK_NULLPTR);
  errid = clEnqueueNDRangeKernel(m_Manager->GetCommandQueue(
                                   m_CommandQueueId), m_KernelContainer[kernelIdx], 3, ITK_NULLPTR, gws, ITK_NULLPTR, 0, ITK_NULLPTR, ITK_NULLPTR);
  OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);

  if(errid != CL_SUCCESS)
    {
    itkWarningMacro("GPU kernel launch failed");
    return false;
    }

  return true;
}

bool GPUKernelManager::LaunchKernel(int kernelIdx, int dim, size_t *globalWorkSize, size_t *localWorkSize)
{
  if(kernelIdx < 0 || kernelIdx >= (int)m_KernelContainer.size() ) return false;

  if(!CheckArgumentReady(kernelIdx) )
    {
    itkWarningMacro("GPU kernel arguments are not completely assigned");
    return false;
    }

// debug
//std::cout << "Dim : " << dim << std::endl;

// debug - if devicetype is CPU
//localWorkSize[0] = localWorkSize[1] = localWorkSize[2] = 1;
//

  cl_int errid;
  errid = clEnqueueNDRangeKernel(m_Manager->GetCommandQueue(
                                   m_CommandQueueId), m_KernelContainer[kernelIdx], (cl_uint)dim, ITK_NULLPTR, globalWorkSize,
                                    localWorkSize, 0, ITK_NULLPTR, ITK_NULLPTR);
  OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);

/*
std::cout << "Check point 1" << std::endl;

// debug -- synchronize
errid = clFlush(m_Manager->GetCommandQueue(m_CommandQueueId));
OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);

std::cout << "Check point 2" << std::endl;
*/
errid = clFinish(m_Manager->GetCommandQueue(m_CommandQueueId));
OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);
/*
std::cout << "Wait for kernel execution ends" << std::endl;
*/

  if(errid != CL_SUCCESS)
    {
    itkWarningMacro("GPU kernel launch failed");
    return false;
    }

  return true;
}

void GPUKernelManager::SetCurrentCommandQueue( int queueid )
{
  if( queueid >= 0 && queueid < (int)m_Manager->GetNumberOfCommandQueues() )
    {
    // Assumption: different command queue is assigned to different device
    m_CommandQueueId = queueid;
    }
  else
    {
    itkWarningMacro("Not a valid command queue id");
    }
}

int GPUKernelManager::GetCurrentCommandQueueID()
{
  return m_CommandQueueId;
}

}
