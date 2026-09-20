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

## Building an external module against an installed ITK

An external module can be configured, built, and Python-wrapped against an
installed ITK prefix rather than an ITK build tree. Point `ITK_DIR` at the
installed package directory, for example
`<prefix>/lib/cmake/ITK-6.0`, and configure the module as usual.

Wrapping additionally requires the SWIG type indices and the wrapping CMake
infrastructure, which ITK installs only on request because the payload is
tens of megabytes. To make an installed prefix usable as a wrapping SDK, the
ITK being installed must be configured with:

```bash
cmake -DITK_WRAP_PYTHON:BOOL=ON \
      -DITK_INSTALL_WRAPPING_DEVELOPMENT_FILES:BOOL=ON <itk-source>
```

then rebuilt and reinstalled. Configuring a module with `ITK_WRAP_PYTHON=ON`
against a prefix that lacks these files fails with "Could not find wrapping
infrastructure"; the remedy is to reconfigure, rebuild, and reinstall the ITK
the module points at, not to change anything in the module.

## Further Reading

For more information visit README documentation at the [ITKRemoteModuleBuildTestPackageAction](https://github.com/InsightSoftwareConsortium/ITKRemoteModuleBuildTestPackageAction/blob/main/README.md#itkremotemodulebuildtestpackageaction) project.
