# ==========================================================================
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
# ==========================================================================*/

import copy
import numpy as np

import itk

itk.auto_progress(2)


def calculate_features(lbl_img, int_img):
    filt = itk.LabelImageToStatisticsLabelMapFilter[type(lbl_img), type(int_img), itk.LabelMap.x3].New()
    filt.SetInput1(lbl_img)
    filt.SetInput2(int_img)
    filt.SetComputeHistogram(False)
    filt.SetComputeFeretDiameter(True)
    filt.SetComputePerimeter(True)
    filt.Update()
    features = filt.GetOutput()
    computed_means = np.array([features[l].GetMean() for l in features.GetLabels()])
    computed_medians = np.array([features[l].GetMedian() for l in features.GetLabels()])
    assert np.array_equal(computed_means, np.array([254.48, 257.0, 232.56, 239.16, 258.2, 224.36,]),)
    assert np.array_equal(computed_medians, np.array([267.0, 231.0, 172.0, 203.0, 209.0, 237.0,]),)


shape = (50, 50, 1)
# Define a dummy label image
lbl_img_np = np.zeros(shape).astype("uint16")
lbl_img_np[5:10, 5:10, 0] = 1
lbl_img_np[15:20, 5:10, 0] = 2
lbl_img_np[25:30, 5:10, 0] = 3
lbl_img_np[5:10, 15:20, 0] = 4
lbl_img_np[15:20, 15:20, 0] = 5
lbl_img_np[25:30, 15:20, 0] = 6

# Define a dummy intensity image
np.random.seed(20210413)
int_img_np = np.random.randint(500, size=shape).astype("uint16")

# Set some values very high to see if that causes the bug to reproduce
int_img_np_outlier = copy.deepcopy(int_img_np)
int_img_np_outlier[1, 1, 0] = 62000
int_img_np[1, 1, 0] = 100

lbl_img = itk.GetImageFromArray(lbl_img_np)
int_img = itk.GetImageFromArray(int_img_np)
int_img_outlier = itk.GetImageFromArray(int_img_np_outlier)

# print("Regular image")
calculate_features(lbl_img, int_img)

# print("Image with 1 outlier pixel (outside the measurement area)")
calculate_features(lbl_img, int_img_outlier)
