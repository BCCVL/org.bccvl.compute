#!/usr/bin/env python

import sys
import os
sys.path.append(os.path.dirname(os.path.realpath(__file__))+"/../")

import unittest
import variable

# Here's our "unit tests".
class VariableTests(unittest.TestCase):
    
    def test_double_range_basic(self):
        import json
        d=json.loads(open("params.json").read())
        params = { "min": 0, "max": 1, "json_dict_path": ["params", "rang"], "n":10}
        s = variable.double_range(d, params)
        #print s.every_choice()
        #print s.random_choice()
    
    def test_integer_range_basic(self):
        import json
        d=json.loads(open("params.json").read())
        params = { "min": 10, "max": 100, "json_dict_path": ["params", "maxit"], "step":3}
        s = variable.integer_range(d, params)
        #print s.every_choice()
        #print s.random_choice()

    def test_subset_basic(self):
        import json
        d=json.loads(open("params.json").read())
        params = { "min": 1, "max": 1, "json_dict_path": ["params", "environmental_datasets"] }
        s = variable.subset(d, params)
        #print s.every_choice()

def main():
    unittest.main()

if __name__ == '__main__':
    main()

