#==========================================================================
#
#   Copyright Insight Software Consortium
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/

from __future__ import print_function

import itk
from sys import argv, exit
itk.auto_progress(2)

if argv[2] == "Ball":
    print("Ball")
    strel = itk.FlatStructuringElement[2].Ball(int(argv[3]))
elif argv[2] == "Box":
    print("Box")
    strel = itk.FlatStructuringElement[2].Box(int(argv[3]))
elif argv[2] == "FromImage":
    print("FromImage")
    reader = itk.ImageFileReader.IUC2.New(FileName=argv[3])
    strel = itk.FlatStructuringElement[2].FromImageUC(reader.GetOutput())
else:
    print("invalid arguement: " + argv[2])
    exit(1)

img = strel.GetImageUC()
size = itk.size(img)
for y in range(0, size.GetElement(1)):
    for x in range(0, size.GetElement(0)):
        if img.GetPixel([x, y]):
            print("X")
        else:
            print(" ")
    print("\n")

itk.write(img, argv[1])

# writer = itk.ImageFileWriter.IUC2.New(FileName=argv[1], Input=img )
# itk.echo(writer)
# writer.Update()
