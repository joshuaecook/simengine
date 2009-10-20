from distutils.core import setup, Extension

simex = Extension('simex_helper',
                  sources=['src/simex_helper.c'],
                  include_dirs=['../include', '/opt/python/lib/python2.4/site-packages/numpy/core/include'])
setup(name='simEngine',
      version='0.91',
      description='The high-performance software simulation engine.',
      ext_modules=[simex])
