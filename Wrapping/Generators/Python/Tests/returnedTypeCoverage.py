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
import re
import sys
from itkTemplate import itkTemplate
from optparse import OptionParser


ctypes = [
    'double',
    'float',
    'signed_char',
    'signed_short',
    'signed_long',
    'unsigned_char',
    'unsigned_short',
    'unsigned_long',
    'char',
    'short',
    'long',
    'bool',
    'int',
    'unsigned_int',
    'void'
]


excludedMethodsList = [
    'Delete',
    'Unregister',
    'SetReferenceCount',

    # the method broken for all filters
    'PushBackInput',
    'PushFrontInput',
    'GraftOutput',
    'SetInput',
    'UpdateOutputData',
    'PropagateRequestedRegion',
    'EnlargeOutputRequestedRegion',

    # functor are not wrapped so exclude GetFunctor
    'GetFunctor',

    #'UnCreateAllInstanceRegister',
    #'Update',
    #'UpdateLargestPossibleRegion',
    #'GenerateInputRequestedRegion',
    #'GenerateOutputInformation',
    #'GenerateData',
    #'GetImageIO',
    #'UpdateOutputData',
    #'UpdateOutputInformation',
    #'GraftOutput',
    #'AddSeed',
    #'GetInputs',
    #'GetOutputs',
    #'CreateAllInstance'
]

excludedMethodsWithParamList = [
    #'GetElement',
    #'RegisterFactory',
    #'UnRegisterFactory',
    #'PropagateRequestedRegion',
    #'Resize',
    #'PadByRadius'
]

excludedClasses = [
    #'BinaryBallStructuringElement',
    #'Neighborhood',
    #'BSplineDecompositionImageFilter',
    #'AtanImageFilter',
    #'Atan2ImageFilter',
    #'ChangeInformationImageFilter',
    #'GeodesicActiveContourLevelSetImageFilter',
    #'Image',
    #'ImageBase',
    #'ImageFileReader',
    #'ImageFileWriter',
    #'ImageRegistrationMethod',
    #'BSplineDownsampleImageFilter',
    #'BSplineUpsampleImageFilter',
    #'CropImageFilter',
    #'LevelSetFunction',
    #'MattesMutualInformationImageToImageMetric',
    #'MeanReciprocalSquareDifferenceImageToImageMetric',
    #'MeanSquaresImageToImageMetric',
    #'MultiResolutionImageRegistrationMethod',
    #'MultiResolutionPyramidImageFilter',
    #'MutualInformationImageToImageMetric',
    #'NormalizedCorrelationImageToImageMetric',
    #'ParallelSparseFieldLevelSetImageFilter',
    #'PyBuffer',
    #'RawImageIO',
    #'RecursiveMultiResolutionPyramidImageFilter',
    #'ResampleImageFilter',
    #'RescaleIntensityImageFilter',
    #'SegmentationLevelSetImageFilter',
    #'ShapeDetectionLevelSetImageFilter',
    #'SparseFieldFourthOrderLevelSetImageFilter',
    #'SparseFieldLevelSetImageFilter',
    #'SpatialObject',
    #'SpatialObjectTreeNode',
    #'WatershedImageFilter',
    #'TreeNode',
    #'ThresholdSegmentationLevelSetImageFilter',
    #'PointSet',
    #'ImageConstIterator',
    #'ImageConstIteratorWithIndex',
    #'ImageIterator',
    #'ImageIteratorWithIndex',
    #'ImageLinearConstIteratorWithIndex',
    #'ImageLinearIteratorWithIndex',
    #'ImageRandomConstIteratorWithIndex',
    #'ImageRandomIteratorWithIndex',
    #'ImageRandomNonRepeatingConstIteratorWithIndex',
    #'ImageRandomNonRepeatingIteratorWithIndex',
    #'ImageRegionConstIterator',
    #'ImageRegionIterator',
    #'ImageRegionConstIteratorWithIndex',
    #'ImageRegionIteratorWithIndex',
    #'InterpolateImageFilter',
    #'IsolatedConnectedImageFilter',
    'SmartPointer',
    'BMPImageIO',
]


def log(s, level):
    if level <= options.verbose:
        print(logFile, s)
        logFile.flush()


def cleanType(s):
    i = s.index('_', 1)
    return s[i + 1:]


def addUnwrappedType(s):
    s = cleanType(s)
    if not s in unwrappedTypes:
        unwrappedTypes.add(s)
        log(s, 0)


def exploreTpl(tpl):
    for cl in tpl.itervalues():
        exploreMethods(cl, cl)
        # try to instanciate the class
        try:
            obj = cl.New()
            exploreMethods(obj, cl)
        except:
            pass
        try:
            exploreMethods(cl(), cl)
        except:
            pass


def exploreMethods(obj, cl):
    isin = isinstance(i, str)
    ls = excludedMethodsList
    attrNameList = sorted(
        [i for i in dir(obj) if isin and i[0].isupper() and i not in ls])

    for attrName in attrNameList:
        log(" + " + attrName, 2)
        try:
            parameters = repr((cl.__name__, attrName))
            if parameters not in exclude:
                log(parameters, 4)
                exec "s = obj.%s()" % attrName
                if isUnwrappedTypeString(s):
                    addUnwrappedType(s)
                    log("   - " + cleanType(s), 5)
        except:
            # try with some parameters
            if attrName not in excludedMethodsWithParamList:
                for param in [0, '', False, None]:
                    parameters = repr((cl.__name__, attrName, param))
                    if parameters not in exclude:
                        log('  * ' + repr(param), 3)
                        log(parameters, 4)
                        try:
                            exec "s = obj.%s(param)" % attrName
                            if isUnwrappedTypeString(s):
                                addUnwrappedType(s)
                                log("   - " + cleanType(s), 5)
                        except:
                            pass


def isUnwrappedTypeString(s):
    if not isinstance(s, str):
        return False
    if not s[0] == "_":
        return False
    for t in ctypes:
        if re.match('^_[0-9a-z]+_p_%s$' % t, s):
            return False
    return True


parser = OptionParser(usage="usage: %prog")
parser.add_option(
    "--exclude",
    dest="exclude",
    default=None,
    metavar="FILE",
    help="")
parser.add_option(
    "--log-file",
    dest="logFile",
    default="-",
    metavar="FILE",
    help="")
parser.add_option(
    "--start-from",
    dest="startFrom",
    default=None,
    metavar="CLASS",
    help="")
parser.add_option(
    "-v",
    "--verbose",
    dest="verbose",
    default=0,
    type="int",
    help="")
(options, args) = parser.parse_args()

if options.logFile == "-":
    logFile = sys.stdout
else:
    logFile = file(options.logFile, "w")


exclude = set()
if options.exclude:
    exclude = set(file(options.exclude).read().splitlines())

val = [i for i in dir(itk) if i[0].isupper() and len(i) > 2]
attrNameList = sorted(set(val) - set(excludedClasses))

if options.startFrom:
    attrNameList = attrNameList[attrNameList.index(options.startFrom):]

unwrappedTypes = set()


for name in attrNameList:
    exec "attr = itk." + name
    # attr = itk.__dict__[name]
    log(name, 1)
    if isinstance(attr, itkTemplate):
        exploreTpl(attr)
    else:
        exploreMethods(attr, attr)
        try:
            exploreMethods(attr.New(), attr)
        except:
            pass
        try:
            exploreMethods(attr(), attr)
        except:
            pass
