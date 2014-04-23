#!/usr/bin/env python

"""VisualizeTestTimes.py

Visualize CTest test times with VTK.

Run from the top of the build tree after the ctest has been run at least once.

Pass the --modular-grouping flag to group by module."""

from __future__ import print_function

import os
import pprint
import subprocess
import sys
import vtk

vtk_major_version = vtk.vtkVersion.GetVTKMajorVersion()

if len(sys.argv) > 1 and sys.argv[1] == '-h':
    print('Usage: VisualizeTestTimes.py [--modular-grouping]')
    sys.exit(1)
modular = False
if len(sys.argv) > 1 and sys.argv[1] == '--modular-grouping':
    modular = True

# Sanity check to ensure we are in the build tree
test_cost_data_file = os.path.join('Testing', 'Temporary', 'CTestCostData.txt')
if not os.path.exists(test_cost_data_file):
    print('Run this script from the build tree after running ctest ' +
          'at least once.')
    sys.exit(1)

# Read the input data
with open(test_cost_data_file, 'r') as fp:
    test_cost_data_lines = fp.readlines()
failed_tests_index = test_cost_data_lines.index('---\n')

# Import the data into a vtkTable
table = vtk.vtkTable()
id_array = vtk.vtkUnsignedIntArray()
id_array.SetName('Pedigree Id')
table.AddColumn(id_array)
attributes = table.GetAttributes(vtk.vtkDataObject.ROW)
attributes.SetActivePedigreeIds('Pedigree Id')
test_name_array = vtk.vtkStringArray()
test_name_array.SetName('Test Name')
table.AddColumn(test_name_array)
number_of_runs_array = vtk.vtkUnsignedIntArray()
number_of_runs_array.SetName('Number of Runs')
table.AddColumn(number_of_runs_array)
test_time_array = vtk.vtkFloatArray()
test_time_array.SetName('Test Time')
table.AddColumn(test_time_array)
runs_long_array = vtk.vtkStringArray()
runs_long_array.SetName('RUNS_LONG Label')
table.AddColumn(runs_long_array)
other_labels_array = vtk.vtkStringArray()
other_labels_array.SetName('Other Labels')
table.AddColumn(other_labels_array)
table.SetNumberOfRows(failed_tests_index)
ctest_exe = 'ctest'
runs_long = subprocess.check_output([ctest_exe, '-L', 'RUNS_LONG', '-N'])
runs_long = runs_long.split('\n')[1:-3]
runs_long = [ii.split()[2] for ii in runs_long]
has_runs_long_time = 0.0
no_runs_long_time = 0.0
for ii in range(failed_tests_index):
    split = test_cost_data_lines[ii].strip().split()
    table.SetValue(ii, 0, ii)
    name = split[0]
    table.SetValue(ii, 1, name)
    table.SetValue(ii, 2, int(split[1]))
    time = float(split[2])
    table.SetValue(ii, 3, time)
    if name in runs_long:
        table.SetValue(ii, 4, 'Has RUNS_LONG Label')
        has_runs_long_time += time
    else:
        table.SetValue(ii, 4, 'No RUNS_LONG Label')
        no_runs_long_time += time
    table.SetValue(ii, 5, 'None')
labels = subprocess.check_output([ctest_exe, '--print-labels'])
labels = labels.split('\n')[2:-1]
labels = [ii.strip() for ii in labels]
if 'RUNS_LONG' in labels:
    labels.pop(labels.index('RUNS_LONG'))
# Assuming tests will only have RUNS_LONG and up to only one other label
if modular:
    for label in labels:
        tests = subprocess.check_output([ctest_exe, '-L', label, '-N'])
        tests = tests.split('\n')[2:-3]
        tests = [ii.split()[2] for ii in tests]
        for test in tests:
            index = test_name_array.LookupValue(test)
            other_labels_array.SetValue(index, label)
print('RUNS_LONG tests:')
pprint.pprint(runs_long)
print('RUNS_LONG time percentage: {0:.4}%'.format(str(has_runs_long_time /
      (has_runs_long_time + no_runs_long_time) * 100)))
print('RUNS_LONG test percentage: {0:.4}%'.format(str((len(runs_long)) /
      (failed_tests_index - 1.0) * 100)))

# Convert the vtkTable to a vtkTree
table_to_tree = vtk.vtkTableToTreeFilter()
if vtk_major_version is 5:
    table_to_tree.SetInput(table)
else:
    table_to_tree.SetInputData(table)
group_runs_long = vtk.vtkGroupLeafVertices()
group_runs_long.SetInputConnection(table_to_tree.GetOutputPort())
group_runs_long.SetInputArrayToProcess(0, 0, 0, vtk.vtkDataObject.VERTEX,
                                       'RUNS_LONG Label')
group_runs_long.SetInputArrayToProcess(1, 0, 0, vtk.vtkDataObject.VERTEX,
                                       'Test Name')
group_other_label = vtk.vtkGroupLeafVertices()
group_other_label.SetInputConnection(group_runs_long.GetOutputPort())
group_other_label.SetInputArrayToProcess(0, 0, 0, vtk.vtkDataObject.VERTEX,
                                         'Other Labels')
group_other_label.SetInputArrayToProcess(1, 0, 0, vtk.vtkDataObject.VERTEX,
                                         'Test Name')

# Visualize with a tree map view
tree_map_view = vtk.vtkTreeMapView()
if modular:
    tree_map_view.SetTreeFromInputConnection(group_other_label.GetOutputPort())
else:
    tree_map_view.SetTreeFromInputConnection(group_runs_long.GetOutputPort())
tree_map_view.SetAreaLabelArrayName('Test Name')
tree_map_view.SetAreaHoverArrayName('Test Name')
tree_map_view.SetAreaLabelVisibility(True)
tree_map_view.SetAreaSizeArrayName('Test Time')
tree_map_view.DisplayHoverTextOn()
tree_map_view.SetLayoutStrategyToSquarify()

# Pretty preference: Mellow, Neon, Ocean
theme = vtk.vtkViewTheme.CreateNeonTheme()
tree_map_view.ApplyViewTheme(theme)

tree_map_view.Update()
tree_map_view.ResetCamera()

interactor = tree_map_view.GetInteractor()
interactor.Initialize()
interactor.Start()
