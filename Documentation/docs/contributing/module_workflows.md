# ITK External Module Continuous Integration

The Insight Software Consortium [ITKRemoteModuleBuildTestPackageAction](https://github.com/InsightSoftwareConsortium/ITKRemoteModuleBuildTestPackageAction/blob/main/README.md) project provides reusable GitHub Actions workflows to support ITK module development.

Reusable ITK workflows provide the following features across Linux, macOS, and Windows platforms:
- Automated C++ build validation
- Automated C++ testing
  - Unit testing
  - Style enforcement
- Automated Python build validation
- Automated Python Jupyter Notebook testing
- Automated Python packaging and uploads

ITK reusable workflows support most ITK external modules and provide CI boilerplate to minimize the development effort required of collaborators.

## Example Usage

It is easy to add ITK reusable workflow integration to an external module. In a file `workflow.yml` in the `.github/workflows` directory of your project, add the following specification:

```yaml
name: Build, test, package

on: [push,pull_request]

jobs:
  cxx-build-workflow:
    uses: InsightSoftwareConsortium/ITKRemoteModuleBuildTestPackageAction/.github/workflows/build-test-cxx.yml@v5.3.0
    with:
      itk-cmake-options: '-DITK_BUILD_DEFAULT_MODULES:BOOL=OFF -DITKGroup_Core:BOOL=ON'

  python-build-workflow:
    uses: InsightSoftwareConsortium/ITKRemoteModuleBuildTestPackageAction/.github/workflows/build-test-package-python.yml@v5.3.0
    with:
      test-notebooks: true
    secrets:
      pypi_password: ${{ secrets.pypi_password }}
```

## Further Reading

For more information visit README documentation at the [ITKRemoteModuleBuildTestPackageAction](https://github.com/InsightSoftwareConsortium/ITKRemoteModuleBuildTestPackageAction/blob/main/README.md#itkremotemodulebuildtestpackageaction) project.
