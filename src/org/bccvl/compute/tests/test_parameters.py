import unittest2 as unittest
from decimal import Decimal
from pkg_resources import resource_string
from zope.interface.verify import verifyClass
from zope.interface.verify import verifyObject
from org.bccvl.site.browser.parameter import ParametersBRT
from org.bccvl.site.browser.parameter import IParameters, IParametersBRT
from jinja2 import Undefined


class IgnoreUndefined(Undefined):

    def __getattr__(self, name):
        return self

    def __getitem__(self, name):
        return self


class BRT_Params_Test(unittest.TestCase):

    def _getTargetClass(self):
        return ParametersBRT

    def _makeOne(self):
        inst = self._getTargetClass()()
        return inst

    def test_class_conforms_to_IParameters(self):
        klass = self._getTargetClass()
        verifyClass(IParameters, klass)
        verifyClass(IParametersBRT, klass)

    def test_instance_conforms_IParameters(self):
        inst = self._makeOne()
        verifyObject(IParameters, inst)
        #verifyObject(IParametersBRT, inst)

    def test_parameter_template(self):
        from jinja2 import Template
        brt_config = resource_string('org.bccvl.compute', 'rscripts/brt.init.R')
        tmpl = Template(brt_config)
        # ignore undefined ... we'll only check certain values
        tmpl.environment.undefined = IgnoreUndefined
        inst = self._makeOne()
        params = {
            'tree_complexity': inst.tree_complexity,
            'learning_rate': inst.learning_rate,
            'family': inst.family
            }

        script = tmpl.render(params)
        self.assertIn('brt.tree.complexity = 1 ', script)
        self.assertIn('brt.learning.rate = 0.01 ', script)
        self.assertIn('brt.family = "bernoulli" ', script)
