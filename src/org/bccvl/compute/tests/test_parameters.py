import unittest2 as unittest
from decimal import Decimal
from pkg_resources import resource_string
from zope.interface.verify import verifyClass
from zope.interface.verify import verifyObject
from org.bccvl.compute.brt import ParametersBRT
from org.bccvl.compute.brt import IParametersBRT


class BRT_Params_Test(unittest.TestCase):

    def _getTargetClass(self):
        return ParametersBRT

    def _makeOne(self):
        inst = self._getTargetClass()()
        return inst

    def test_class_conforms_to_IParameters(self):
        klass = self._getTargetClass()
        verifyClass(IParametersBRT, klass)

    def test_instance_conforms_IParameters(self):
        inst = self._makeOne()
        verifyObject(IParametersBRT, inst)
