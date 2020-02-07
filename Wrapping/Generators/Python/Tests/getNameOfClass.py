#==========================================================================
#
#   Copyright NumFOCUS
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

# a short program to check the value returned by the GetNameOfClass() methods

import itk
import sys
itk.auto_progress(2)

# must force the load to return all the names with dir(itk)
itk.force_load()
# itk.ImageToImageFilter


def wrongClassName(cl, name):
    o = cl.New()
    # be sure that the type of the instantiated object is the same
    # than the one of the class. It can be different if the class
    # is an "abstract" one and don't provide any New() method.
    # In that case, the one of the superclass is used.
    return o.GetNameOfClass() != name and itk.class_(o) == cl

# a list of classes to exclude. Typically, the classes with a custom New()
# method, which return a subclass of the current class
exclude = ["ForwardFFTImageFilter",
           "InverseFFTImageFilter",
           "OutputWindow",
           "MultiThreaderBase",
           "FFTComplexToComplexImageFilter",
           "ComplexToComplexFFTImageFilter",
           "templated_class",
           "HalfHermitianToRealInverseFFTImageFilter",
           "RealToHalfHermitianForwardFFTImageFilter",
           "CustomColormapFunction",
           "ScanlineFilterCommon"  # Segfault
           ]

wrongName = 0
totalName = 0

for t in dir(itk):
    if t not in exclude:
        T = itk.__dict__[t]
        # first case - that's a templated class
        if isinstance(T, itk.Vector.__class__) and len(T) > 0:
            # use only the first specialization - all of them return the same
            # name
            i = T.values()[0]
            # GetNameOfClass() is a virtual method of the LightObject class,
            # so we must instantiate an object with the New() method
            if 'New' in dir(i) and 'GetNameOfClass' in dir(i):
                totalName += 1
                if wrongClassName(i, t):
                    msg = "%s: wrong class name: %s" % (t, n)
                    print(msg, file=sys.stderr)
                    wrongName += 1
        else:
            if 'New' in dir(T) and 'GetNameOfClass' in dir(T):
                totalName += 1
                if wrongClassName(T, t):
                    msg = "%s: wrong class name: %s" % (t, n)
                    print(msg, file=sys.stderr)
                    wrongName += 1

print("%s classes checked." % totalName)
if wrongName:
    print(
        "%s classes are not providing the correct name." % wrongName,
        file=sys.stderr)
    sys.exit(1)
