# See http://peak.telecommunity.com/DevCenter/setuptools#namespace-packages
try:
    __import__('pkg_resources').declare_namespace(__name__)
except (ImportError, AttributeError):
    # AttributeError may be caused due to recursive import of pkg_resources
    # let's fall back to legacy namespace packages in that case
    from pkgutil import extend_path
    __path__ = extend_path(__path__, __name__)
