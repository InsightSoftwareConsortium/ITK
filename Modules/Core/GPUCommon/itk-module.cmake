set(DOCUMENTATION "This module contains the framework for processing images
with the GPU.  This includes base classes for the GPU image filters, some
OpenCL utilities, and classes to manage the interface between the CPU and
the GPU.  These classes manage the GPU kernel, transferring the data to and
from the GPU, and managing the GPU contexts.")

itk_module(ITKGPUCommon
  DEPENDS
    ITKCommon
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
