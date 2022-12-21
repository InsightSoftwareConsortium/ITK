# -*- coding: utf-8 -*-
from __future__ import print_function
from os import sys

try:
    from skbuild import setup
except ImportError:
    print('scikit-build is required to build from source.', file=sys.stderr)
    print('Please run:', file=sys.stderr)
    print('', file=sys.stderr)
    print('  python -m pip install scikit-build')
    sys.exit(1)

setup(
    name='itk-bonemorphometry',
    version='1.4.0',
    author='Jean-Baptiste Vimort',
    author_email='jb.vimort@kitware.com',
    packages=['itk'],
    package_dir={'itk': 'itk'},
    download_url=r'https://github.com/InsightSoftwareConsortium/ITKBoneMorphometry',
    description=r'An ITK module to compute bone morphometry features and feature maps',
    long_description='ITKBoneMorphometry provides bone analysis filters that '
                     'compute features from N-dimensional images that '
                     'represent the internal architecture of bone.\n'
                     'Please refer to:'
                     'Vimort J., McCormick M., Paniagua B.,'
                     '“Computing Bone Morphometric Feature Maps from 3-Dimensional Images”, '
                     'Insight Journal, January-December 2017, https://hdl.handle.net/10380/3588.',
    classifiers=[
        "License :: OSI Approved :: Apache Software License",
        "Programming Language :: Python",
        "Programming Language :: C++",
        "Development Status :: 4 - Beta",
        "Intended Audience :: Developers",
        "Intended Audience :: Education",
        "Intended Audience :: Healthcare Industry",
        "Intended Audience :: Science/Research",
        "Topic :: Scientific/Engineering",
        "Topic :: Scientific/Engineering :: Medical Science Apps.",
        "Topic :: Scientific/Engineering :: Information Analysis",
        "Topic :: Software Development :: Libraries",
        "Operating System :: Android",
        "Operating System :: Microsoft :: Windows",
        "Operating System :: POSIX",
        "Operating System :: Unix",
        "Operating System :: MacOS"
        ],
    license='Apache',
    keywords='ITK InsightToolkit bones morphometry',
    url=r'https://github.com/InsightSoftwareConsortium/ITKBoneMorphometry',
    install_requires=[
        r'itk>=5.3.0'
    ]
    )
