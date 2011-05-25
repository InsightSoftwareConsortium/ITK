set("DOCUMENTATION  This module contains the basic infrastructure to support
GPU based operations in ITK. The essential classes here allow to make
transparent use of an itkImage with memory managed either by the CPU or the
GPU. They also make possible to provide GPU-based filters that act as
replacements for existing standard CPU filters. This replacement is done by
taking advantage of the ITK Factories, and have the advantage that the code of
existing applications do not have to be modified in order to take advantage of
this GPU support.")

itk_module(ITK-GPUCommon DEPENDS  ITK-Common OpenCL TEST_DEPENDS ITK-TestKernel DESCRIPTION "${DOCUMENTATION}")
