# 'org' is a Jython package, stdlib copy tries to import that.
# if we would use pkg_resources.declare_namespace we would create a
# recursive import chain, because pkg_resources tries to import copy which
# tries to import this module etc.....
__path__ = __import__('pkgutil').extend_path(__path__, __name__)
