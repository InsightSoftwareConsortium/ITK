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
import types
itk.auto_progress(2)

# must force the load to return all the names with dir(itk)
itk.force_load()

wrongName = 0
totalName = 0

# a list of classes to exclude. Typically, the classes with a custom New()
# method, which return a subclass of the current class
exclude = [
"ScanlineFilterCommon",  # Segfault
"templated_class",
"auto_pipeline",
"pipeline"
]


def create_and_test(t, create_method):
    wrongType = 0
    # Local scope in which objects are created.
    if create_method == 'New':
        I = t.New()
    elif create_method == 'call':
        try:
            I = t()
        except:  # Call function needs parameters or class is abstract
            return 0, 0
    else:
        return 0, 0  # Do not add anything to `totalName` and `wrongName`.
    obj_type = itk.python_type(I)
    # If class is virtual, actual type could be different from expected type
    actual_type = type(I)
    totalName = 1
    # Check that there is no C++ character left in Python obj_type
    if any(substring in obj_type for substring in ['<', ':', '>', 'class', 'itkTemplate', 'std', ' ', '(', ')', 'itkCType']):
            msg = "%s: wrong Python class name: %s" % (t, obj_type)
            wrongType = 1
    else:
        try:
            if eval(obj_type) != actual_type:
                msg = "%s: wrong Python class name: %s" % (actual_type, obj_type)
                wrongType = 1
        except Exception as e:
            msg = ("%s: wrong Python class name: %s. "
            "Exception while evaluating it: %s" %
            (t, obj_type, e.message))
            wrongType = 1
    if wrongType:
        print(msg, file=sys.stderr)
    return totalName, wrongType

def create_method(i):
    if 'New' in dir(i):
        return 'New'
    elif '__call__' in dir(i) and not isinstance(i, types.FunctionType):
        return 'call'
    else:
        return None

for t in dir(itk):
    if t not in exclude:
        T = itk.__dict__[t]
        # first case - that's a templated class
        if isinstance(T, itk.Vector.__class__) and len(T) > 0:
            for k in T.keys():
                i = T[k]
                # GetNameOfClass() is a virtual method of the LightObject class,
                # so we must instantiate an object with the New() method
                t, w = create_and_test(i, create_method(i))
                wrongName += w
                totalName += t
        else:
            t, w = create_and_test(T, create_method(T))
            wrongName += w
            totalName += t

print("%s classes checked." % totalName)
if wrongName:
    print(
        "%s classes are not providing the correct Python class name." % wrongName,
        file=sys.stderr)
    sys.exit(1)

# Also test for Python types:
assert "int" == itk.python_type(int)
assert "int" == itk.python_type(3)
assert "list" == itk.python_type(list)
assert "list" == itk.python_type([])
