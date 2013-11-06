import unittest2 as unittest
from plone.app.uuid import utils
from org.bccvl.site.content.experiment import Experiment
from org.bccvl.site.content.dataset import Dataset
from org.bccvl.site.browser.parameter import ParametersBRT
import os
import pkg_resources
from decimal import Decimal


def gen_mock_uuidToObject(data):

    def mock_uuidToObject(uuid):
        return data[uuid]

    return mock_uuidToObject


class MockFile(object):


    filename = None

    def __init__(self, fullpath):
        self.fullpath = fullpath
        self.filename = os.path.basename(fullpath)

    def open(self):
        return open(self.fullpath, 'r')


class Brt_Test(unittest.TestCase):

    def setUp(self):
        data = {}
        data['1'] = exp = Experiment()
        data['2'] = Dataset()
        data['3'] = Dataset()
        data['4'] = Dataset()
        exp.species_occurrence_dataset = '2'
        exp.species_absence_dataset = '3'
        exp.environmental_dataset = '4'
        data['2'].file = MockFile(pkg_resources.resource_filename(__name__, 'data/occurrence.csv'))
        data['3'].file = MockFile(pkg_resources.resource_filename(__name__, 'data/absence.csv'))
        data['4'].file = MockFile(pkg_resources.resource_filename(__name__, 'data/current_test.zip'))
        params = ParametersBRT()
        # params.tree_complexity = 50
        # params.learning_rate = Decimal('0.01')
        # params.bag_fraction = Decimal('0.75')
        # params.var_monotone = -1
        # params.n_folds = 10
        # params.prev_stratify = True
        # params.family = 'bernoulli'
        # params.n_trees = 50
        # params.max_trees = 1000
        # params.tolerance_method = 'auto'
        # params.tolerance_value = Decimal('0.001')
        exp.parameters_brt = params
        self.experiment = exp
        self.old_uuidToObject = utils.uuidToObject
        utils.uuidToObject = gen_mock_uuidToObject(data)
        # TODO mockup uuid2object

    def tearDown(self):
        self.experiment = None
        utils.uuidToObject = self.old_uuidToObject

    def test_brt(self):
        from org.bccvl.compute.brt import execute
        from org.bccvl.compute.utils import WorkEnvLocal
        execute(self.experiment, WorkEnvLocal)
