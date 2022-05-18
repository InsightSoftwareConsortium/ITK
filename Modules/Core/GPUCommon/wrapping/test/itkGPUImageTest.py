# ==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
# ==========================================================================*/
import itk

width = 256
height = 256

GPUImageType = itk.GPUImage[itk.F, 2]

start = itk.Index[2]()
start[0] = 0
start[1] = 0

size = itk.Size[2]()
size[0] = width
size[1] = height

region = itk.ImageRegion[2](start, size)

srcA = GPUImageType.New()
srcA.SetRegions(region)
srcA.Allocate()
srcA.FillBuffer(1.0)

srcB = GPUImageType.New()
srcB.SetRegions(region)
srcB.Allocate()
srcB.FillBuffer(3.0)

dest = GPUImageType.New()
dest.SetRegions(region)
dest.Allocate()

idx = itk.Index[2]()
idx[0] = 0
idx[1] = 0

number_of_elements = width * height

kernel_manager = itk.GPUKernelManager.New()

gpu_source = itk.GPUImageOps.GetOpenCLSource()
print(gpu_source)

kernel_manager.LoadProgramFromString(gpu_source, "#define PIXELTYPE float\n")

kernel_add = kernel_manager.CreateKernel("ImageAdd")

srcA.SetCurrentCommandQueue(0)
srcB.SetCurrentCommandQueue(0)
dest.SetCurrentCommandQueue(0)
kernel_manager.SetCurrentCommandQueue(0)

print("Current Command Queue ID : 0 ")

print("======================")
print("Kernel : Addition")
print("------------------")
print("Before GPU kernel execution")
print(f"SrcA : {srcA.GetPixel(idx)}")
print(f"SrcB : {srcB.GetPixel(idx)}")
print(f"Dest : {dest.GetPixel(idx)}")

kernel_manager.SetKernelArgWithImage(kernel_add, 0, srcA.GetGPUDataManager())
kernel_manager.SetKernelArgWithImage(kernel_add, 1, srcB.GetGPUDataManager())
kernel_manager.SetKernelArgWithImage(kernel_add, 2, dest.GetGPUDataManager())
kernel_manager.SetKernelArgWithUInt(kernel_add, 3, number_of_elements)
kernel_manager.LaunchKernel2D(kernel_add, width, height, 16, 16)

print("------------------")
print("After GPU kernel execution")
print(f"SrcA : {srcA.GetPixel(idx)}")
print(f"SrcB : {srcB.GetPixel(idx)}")
print(f"Dest : {dest.GetPixel(idx)}")
print("======================")

#
# create multiplication kernel
#
kernel_mult = kernel_manager.CreateKernel("ImageMult")

print("======================")
print("Kernel : Multiplication")
print("------------------")
print("Before GPU kernel execution")
print(f"SrcA : {srcA.GetPixel(idx)}")
print(f"SrcB : {srcB.GetPixel(idx)}")
print(f"Dest : {dest.GetPixel(idx)}")

kernel_manager.SetKernelArgWithImage(kernel_mult, 0, srcA.GetGPUDataManager())
kernel_manager.SetKernelArgWithImage(kernel_mult, 1, srcB.GetGPUDataManager())
kernel_manager.SetKernelArgWithImage(kernel_mult, 2, dest.GetGPUDataManager())
kernel_manager.SetKernelArgWithUInt(kernel_mult, 3, number_of_elements)
kernel_manager.LaunchKernel2D(kernel_mult, width, height, 16, 16)

print("------------------")
print("After GPU kernel execution")
print(f"SrcA : {srcA.GetPixel(idx)}")
print(f"SrcB : {srcB.GetPixel(idx)}")
print(f"Dest : {dest.GetPixel(idx)}")
print("======================")

#
# Change Command Queue if more than one GPU device exists
# otherwise, use same command queue
#
queueID = 0
context_manager = itk.GPUContextManager.GetInstance()
if context_manager.GetNumberOfCommandQueues() >= 2:
    queueID = 1
    print("More than one GPU device available, switching command queues.")
else:
    print("Only one GPU device available, using same command queue.")

print(f"Current Command Queue ID : {queueID}")

#
# create subtraction kernel
#
kernel_sub = kernel_manager.CreateKernel("ImageSub")

srcA.FillBuffer(2.0)
srcB.FillBuffer(4.0)
dest.FillBuffer(1.0)

# default queue id was 0
srcA.SetCurrentCommandQueue(queueID)
srcB.SetCurrentCommandQueue(queueID)
dest.SetCurrentCommandQueue(queueID)
kernel_manager.SetCurrentCommandQueue(queueID)

print("======================")
print("Kernel : Subtraction")
print("------------------")
print("Before GPU kernel execution")
print(f"SrcA : {srcA.GetPixel(idx)}")
print(f"SrcB : {srcB.GetPixel(idx)}")
print(f"Dest : {dest.GetPixel(idx)}")

kernel_manager.SetKernelArgWithImage(kernel_sub, 0, srcA.GetGPUDataManager())
kernel_manager.SetKernelArgWithImage(kernel_sub, 1, srcB.GetGPUDataManager())
kernel_manager.SetKernelArgWithImage(kernel_sub, 2, dest.GetGPUDataManager())
kernel_manager.SetKernelArgWithUInt(kernel_sub, 3, number_of_elements)
kernel_manager.LaunchKernel2D(kernel_sub, width, height, 16, 16)

print("------------------")
print("After GPU kernel execution")
print(f"SrcA : {srcA.GetPixel(idx)}")
print(f"SrcB : {srcB.GetPixel(idx)}")
print(f"Dest : {dest.GetPixel(idx)}")
print("======================")
